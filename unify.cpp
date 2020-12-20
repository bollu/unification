// Dragon book chapter 6, figure 6.18.
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <map>
#include <set>
#include <vector>

#include "parser.h"

using namespace std;

void printIndent(std::ostream &o, int indent) {
    for (int i = 0; i < indent; ++i) {
        o << "\t";
    }
}
extern "C" {
const char *__asan_default_options() { return "detect_leaks=0"; }
}

enum class SurfaceTypeKind { Var, Constructor };
// Name<Arg1, Arg2, ..., Argn> | Name
struct SurfaceType {
    Span span;
    SurfaceTypeKind kind;
    SurfaceType(Span span, SurfaceTypeKind kind) : span(span), kind(kind){};
    virtual void print(std::ostream &o) const = 0;
    // collect the names of variables that occur in this type.
    virtual void collectVarNames(std::set<std::string> &vs) const = 0;
};

struct SurfaceTypeVar : public SurfaceType {
    Identifier name;
    SurfaceTypeVar(Span span, Identifier name)
        : SurfaceType(span, SurfaceTypeKind::Var), name(name){};
    void print(std::ostream &o) const override { o << name.name; }

    void collectVarNames(std::set<std::string> &vs) const override {
        vs.insert(name.name);
    }
};

struct SurfaceTypeConstructor : public SurfaceType {
    Identifier name;
    vector<SurfaceType *> args;
    SurfaceTypeConstructor(Span span, Identifier name,
                           vector<SurfaceType *> args)
        : SurfaceType(span, SurfaceTypeKind::Constructor),
          name(name),
          args(args){};

    void print(std::ostream &o) const override {
        o << "<";
        o << name.name << " ";
        for (int i = 0; i < (int)args.size(); ++i) {
            args[i]->print(o);
            if (i + 1 < (int)args.size()) {
                o << " ";
            }
        }
        o << ">";
    }

    void collectVarNames(std::set<std::string> &vs) const override {
        for (SurfaceType *arg : args) {
            arg->collectVarNames(vs);
        }
    }
};

// a type definition, fully quantified and unique'd.
enum class TcTypeKind { Forall, Constructor, Var };

struct TcType {
    TcTypeKind kind;
    const int uuid;
    virtual void print(ostream &o) const = 0;

    TcType(TcTypeKind kind) : kind(kind), uuid(nuuid_++) {}

   private:
    static int nuuid_;
};

// forall x. <inner>
struct TcTypeFA : public TcType {
    std::string name;
    TcType *inner;
    TcTypeFA(std::string name, TcType *inner)
        : TcType(TcTypeKind::Forall), name(name), inner(inner) {
        assert(islower(name[0]));
    };

    void print(ostream &o) const override {
        o << "(∀ " + getUniqName() + ". ";
        inner->print(o);
        o << ")";
    }

    std::string getUniqName() const { return name + std::to_string(uuid); }
};

// <x>
struct TcTypeVar : public TcType {
    TcTypeFA *fa;  // pointer to the quantifier; de-bruijn-lite
    TcTypeVar(TcTypeFA *fa) : TcType(TcTypeKind::Var), fa(fa){};
    void print(ostream &o) const override { o << fa->getUniqName(); }
};

struct TcTypeConstructor : public TcType {
    std::string name;
    vector<TcType *> args;
    TcTypeConstructor(std::string name, vector<TcType *> args)
        : TcType(TcTypeKind::Constructor), name(name), args(args) {
        // lower is for type vars.
        assert(!islower(name[0]));
    }

    void print(ostream &o) const override {
        o << "<";
        o << name;

        for (int i = 0; i < (int)args.size(); ++i) {
            o << " "; args[i]->print(o);
        }
        o << ">";
    }
};

int TcType::nuuid_ = 0;

// A node in the type checker graph for unification.
enum class UnifNodeKind { Var, Constructor };
struct UnifNode {
    UnifNodeKind kind;
    const int uuid;
    virtual void print(ostream &o, int indent = 0) const = 0;
    UnifNode(UnifNodeKind kind) : kind(kind), uuid(nuuid_++), repr_(this) {}

    UnifNode *repr() const {
        if (this == this->repr_) {
            return this->repr_;
        }
        this->repr_ = this->repr_->repr();
        return this->repr_;
    }

    // subsumes into the equivalence class of `parent`. parent will be the
    // representative of the equivalence class this belongs to.
    // Precondition: This node must be a representative.
    void setEquivRepresentative(UnifNode *parent) {
        assert(repr() == this);
        this->repr_ = parent;
    }

   private:
    mutable UnifNode *repr_;  // representative of equivalence class
    static int nuuid_;
};
struct UnifNodeVar : public UnifNode {
    UnifNodeVar() : UnifNode(UnifNodeKind::Var) {}
    void print(ostream &o, int indent) const override {
        if (this == repr()) {
            o << "[α" << getPronouncableNum(uuid) << "]";
        } else {
            repr()->print(o, indent);
        }
    }
};

struct UnifNodeConstructor : public UnifNode {
    std::string name;
    vector<UnifNode *> children;
    UnifNodeConstructor(std::string name, vector<UnifNode *> children)
        : UnifNode(UnifNodeKind::Constructor), name(name), children(children) {}

    void print(ostream &o, int indent) const override {
        if (this == repr()) {
            o << "[" << name;
            for (UnifNode *u : children) {
                o << " ";
                u->repr()->print(o, indent + 1);
            }
            o << "]";
        } else {
            repr()->print(o, indent);
        }
    }
};

int UnifNode::nuuid_ = 0;

// go from types to unification graph.
// don't take environments by reference, we want the scoping to be correct.
UnifNode *freshGo_(TcType *ty, map<TcTypeFA *, UnifNodeVar *> m);
UnifNode *fresh(TcType *ty) {
    map<TcTypeFA *, UnifNodeVar *> m;
    return freshGo_(ty, m);
}

// forall x. to the fresh node.
UnifNode *freshGo_(TcType *ty, map<TcTypeFA *, UnifNodeVar *> m) {
    if (auto fa = dynamic_cast<TcTypeFA *>(ty)) {
        assert(m.find(fa) == m.end());
        m[fa] = new UnifNodeVar();
        return freshGo_(fa->inner, m);
    }

    if (TcTypeVar *v = dynamic_cast<TcTypeVar *>(ty)) {
        auto it = m.find(v->fa);
        assert(it != m.end() && "must have already seen forall for given var");
        return it->second;
    }

    if (TcTypeConstructor *c = dynamic_cast<TcTypeConstructor *>(ty)) {
        std::vector<UnifNode *> children;
        for (TcType *arg : c->args) {
            children.push_back(freshGo_(arg, m));
        }
        return new UnifNodeConstructor(c->name, children);
    }

    assert(false && "must have handled all Tc nodes!");
};

// unite the nodes `l` and `r` with `l` as the representative.
void unify(UnifNode *l, UnifNode *r, int indent = 0) {
    printIndent(cerr, indent);
    cerr << "\t-{u} ";
    l->print(cerr);
    cerr << "~";
    r->print(cerr);
    cerr << "\n";

    auto printFinalAnswer = [&](std::string ty) {
        printIndent(cerr, indent);
        cerr << "\t-{v} ";
        l->print(cerr);
        cerr << "~";
        r->print(cerr);
        cerr << " | " << ty;
        cerr << "\n";
    };

    l = l->repr();
    r = r->repr();
    if (l == r) {
        printFinalAnswer("eq");
        return;
    }

    if (UnifNodeVar *vl = dynamic_cast<UnifNodeVar *>(l)) {
        vl->setEquivRepresentative(r);
        printFinalAnswer("lvar");
        return;
    }

    if (UnifNodeVar *vr = dynamic_cast<UnifNodeVar *>(r)) {
        vr->setEquivRepresentative(l);
        printFinalAnswer("rvar");
        return;
    }

    UnifNodeConstructor *cl = dynamic_cast<UnifNodeConstructor *>(l);
    assert(cl && "l must be constructor (now) or var (handled previously)");
    UnifNodeConstructor *cr = dynamic_cast<UnifNodeConstructor *>(r);
    assert(cr && "r must be constructor (now) or var (handled previously)");

    // TODO: Why is this necessary? I don't understand.
    cl->setEquivRepresentative(cr);

    if (cl->name != cr->name) {
        cerr << "\n===UNIFICATION ERROR===\n";
        cerr << "mismatched constructors: l|" << cl->name << "| v/s r|"
             << cr->name << "|\n";
        cerr << "\nl: ";
        cl->print(cerr, 0);
        cerr << "\nr: ";
        cl->print(cerr, 0);
        cerr << "\n===\n";
        exit(1);
    }

    if (cl->children.size() != cr->children.size()) {
        cerr << "mismatched number of constructors: |" << cl->children.size()
             << "| v/s |" << cr->children.size() << "|\n";
        cerr << "\nl: ";
        cl->print(cerr, 0);
        cerr << "\nr: ";
        cl->print(cerr, 0);
        cerr << "\n===\n";
        exit(1);
    }

    for (int i = 0; i < cl->children.size(); ++i) {
        unify(cl->children[i], cr->children[i], indent + 1);
    }
    printFinalAnswer("cons");
    return;
};

// decl fnname(t1, t2, ... tn) -> tout
struct FnDecl {
    Span span;
    Identifier name;
    vector<SurfaceType *> argtys;
    SurfaceType *retty;
    TcType *type = nullptr;

    FnDecl(Span span, Identifier name, vector<SurfaceType *> argtys,
           SurfaceType *retty)
        : span(span), name(name), argtys(argtys), retty(retty){};

    void print(std::ostream &o) const {
        o << "decl " << name << "(";
        for (int i = 0; i < (int)argtys.size(); ++i) {
            argtys[i]->print(o);
            if (i + 1 < (int)argtys.size()) {
                o << ", ";
            }
        }
        o << ") -> ";
        retty->print(o);

        o << " : ";
        if (type) {
            type->print(o);
        } else {
            o << " [no type]";
        }
        o << ";";
    }
};

// === Expressions === //
enum class ExprKind { Identifier, Ap };
struct Expr {
    Span span;
    ExprKind kind;
    UnifNode *type = nullptr;
    Expr(Span span, ExprKind kind) : span(span), kind(kind){};
    virtual void print(std::ostream &o, bool withTypes) const = 0;
};

struct ExprIdent : public Expr {
    Identifier name;
    ExprIdent(Span span, Identifier name)
        : Expr(span, ExprKind::Identifier), name(name){};
    void print(std::ostream &o, bool withTypes) const override {
        if (withTypes) {
            assert(type);
            o << name;
            type->repr()->print(o);
        } else {
            o << name;
        }
    }
};

struct ExprAp : public Expr {
    Expr *rator;
    vector<Expr *> rands;
    ExprAp(Span span, Expr *rator, vector<Expr *> rands)
        : Expr(span, ExprKind::Ap), rator(rator), rands(rands){};

    void print(std::ostream &o, bool withTypes) const override {
        if (rator->kind == ExprKind::Identifier) {
            rator->print(o, withTypes);
        } else {
            o << "(";
            rator->print(o, withTypes);
            o << ")";
        }
        o << "(";
        for (int i = 0; i < (int)rands.size(); ++i) {
            rands[i]->print(o, withTypes);
            if (i + 1 < (int)rands.size()) {
                o << ", ";
            }
        }
        o << ")";
        if (withTypes) {
            assert(type);
            o << ":";
            type->repr()->print(o);
        }
    }
};

// === MODULE ===

struct Module {
    std::map<std::string, FnDecl *> decls;
    vector<Expr *> es;

    void print(std::ostream &o) const {
        for (auto it : decls) {
            it.second->print(o);
            o << "\n";
        }

        for (Expr *e : es) {
            e->print(o, /*withTypes=*/false);
            o << ";\n";
        }
    }
};

// === PARSING ===

SurfaceType *parseSurfaceType(Parser &p) {
    Identifier name = p.parseIdentifier();

    if (!isupper(name.name[0]) && !islower(name.name[0])) {
        p.addErr(ParseError(name.span,
                            "expected type name to start with character"));
        assert(false);
        return nullptr;
    }

    if (islower(name.name[0])) {
        return new SurfaceTypeVar(name.span, name);
    } else {
        // Foo
        optional<Span> open = p.parseOptionalSigil("<");
        if (!open) {
            return new SurfaceTypeConstructor(name.span, name, {});
        }

        // Foo <T1, T2, ..., Tn>
        vector<SurfaceType *> vars;
        while (1) {
            vars.push_back(parseSurfaceType(p));
            if (p.parseOptionalComma()) {
                continue;
            } else {
                p.parseMatchingSigil(*open, ">");
                break;
            }
        }
        return new SurfaceTypeConstructor(
            name.span.extendRight(p.getCurrentLoc()), name, vars);
    }
};

// decl fnname(ty1, ty2, .. tyn) -> tyret
FnDecl *parseFnDecl(Span spanBegin, Parser &p) {
    Identifier name = p.parseIdentifier();
    Span open = p.parseOpenRoundBracket();

    vector<SurfaceType *> ts;
    if (!p.parseOptionalCloseRoundBracket()) {
        while (1) {
            ts.push_back(parseSurfaceType(p));
            if (p.parseOptionalComma()) {
                continue;
            } else {
                p.parseCloseRoundBracket(open);
                break;
            }
        }
    }
    p.parseThinArrow();
    SurfaceType *retty = parseSurfaceType(p);
    p.parseSemicolon();
    return new FnDecl(spanBegin.extendRight(p.getCurrentLoc()), name, ts,
                      retty);
}

// expr := apRator apRands
// apRator := '(' expr ')' | ident
// apRands := '(' expr ',' expr ... ',' expr ')' | ε
Expr *parseExpr(Parser &p);
Expr *parseApRator(Parser &p);
vector<Expr *> parseApRands(Parser &p);

Expr *parseApRator(Parser &p) {
    if (optional<Span> open = p.parseOptionalOpenRoundBracket()) {
        Expr *lhs = parseExpr(p);
        p.parseCloseRoundBracket(*open);
        return lhs;
    }
    Identifier name = p.parseIdentifier();
    return new ExprIdent(name.span, name);
}

Expr *parseExpr(Parser &p) {
    Loc begin = p.getCurrentLoc();
    Expr *rator = parseApRator(p);

    optional<Span> open = p.parseOptionalOpenRoundBracket();
    if (!open) { return rator; }

    vector<Expr *> rands;
    if (!p.parseOptionalCloseRoundBracket()) {

        while (1) {
            rands.push_back(parseExpr(p));
            if (p.parseOptionalComma()) {
                continue;
            } else {
                p.parseCloseRoundBracket(*open);
                break;
            }
        }
    }

    Loc end = p.getCurrentLoc();
    return new ExprAp(Span(begin, end), rator, rands);
}

Module parseTopLevel(Parser &p) {
    Module m;
    while (!p.eof()) {
        if (optional<Span> sp = p.parseOptionalKeyword("decl")) {
            FnDecl *decl = parseFnDecl(*sp, p);
            auto it = m.decls.find(decl->name.name);
            if (it != m.decls.end()) {
                cerr << "\n===ERROR: multiple declaration of name |"
                     << decl->name.name << "|==\n";
                p.addNote(
                    ParseError(it->second->span, "original 1st declaration"));
                p.addErr(ParseError(decl->span, "illegal 2nd declaration"));
            } else {
                m.decls[decl->name.name] = decl;
            }
            continue;
        } else {
            m.es.push_back(parseExpr(p));
            p.parseSemicolon();
            continue;
        }
        p.addErr(ParseError(Span(p.getCurrentLoc(), p.getCurrentLoc()),
                            "expected toplevel 'decl' or 'use'"));
    }
    return m;
}

TcType *surfaceType2TcType(SurfaceType *st,
                           std::map<std::string, TcTypeFA *> &vars) {
    if (SurfaceTypeVar *v = dynamic_cast<SurfaceTypeVar *>(st)) {
        assert(vars.count(v->name.name));
        return new TcTypeVar(vars[v->name.name]);
    }

    if (SurfaceTypeConstructor *c =
            dynamic_cast<SurfaceTypeConstructor *>(st)) {
        std::vector<TcType *> args;
        for (SurfaceType *arg : c->args) {
            args.push_back(surfaceType2TcType(arg, vars));
        }
        return new TcTypeConstructor(c->name.name, args);
    }

    assert(false && "surface type must be variable or constructor.");
}

TcType *generateTypeForDecl(const FnDecl *decl) {
    std::set<std::string> vs;
    for (SurfaceType *ty : decl->argtys) {
        ty->collectVarNames(vs);
    }
    decl->retty->collectVarNames(vs);
    std::map<std::string, TcTypeFA *> var2fa;
    for (std::string name : vs) {
        var2fa[name] = new TcTypeFA(name, nullptr);
    }
    // generate type Ap(Prod(t1, t2, ... tn), ret)
    vector<TcType *> prodArgs;
    for (SurfaceType *ty : decl->argtys) {
        prodArgs.push_back(surfaceType2TcType(ty, var2fa));
    }
    TcType *prod = new TcTypeConstructor("π", prodArgs);
    TcType *ret = surfaceType2TcType(decl->retty, var2fa);
    TcType *ap = new TcTypeConstructor("→", {prod, ret});

    TcType *prev = ap;
    for (auto it : var2fa) {
        it.second->inner = prev;
        prev = it.second;
    }

    return prev;
};

void generateTypesForDecls(Module &m) {
    for (auto it : m.decls) {
        it.second->type = generateTypeForDecl(it.second);
    }
}

void typeInferExpr(Expr *e, Module &m, const char *raw_input) {
    if (ExprIdent *id = dynamic_cast<ExprIdent *>(e)) {
        auto it = m.decls.find(id->name.name);
        if (it == m.decls.end()) {
            cerr << printfspan(id->span, raw_input,
                               "ERROR: unable to find idenfier |%s| in module",
                               id->name.name.c_str());
            cerr << printfspan(e->span, raw_input,
                               "ERROR when type checking expression");
            assert(false && "unknown identifier");
        }
        assert(
            it->second->type &&
            "types must have been computed for declarations before inference");
        cerr << "\t-{0} ";
        e->print(cerr, false);
        cerr << "\n";
        e->type = fresh(it->second->type);
        cerr << "\t-{1} ";
        e->print(cerr, true);
        cerr << "\n";
        return;
    }

    if (ExprAp *ap = dynamic_cast<ExprAp *>(e)) {
        typeInferExpr(ap->rator, m, raw_input);
        vector<UnifNode *> argsty;
        for (Expr *rand : ap->rands) {
            typeInferExpr(rand, m, raw_input);
            argsty.push_back(rand->type);
        }

        // rator(rands)
        // t(ap) ~ Ap(Prod(map(t, rands), retty)
        UnifNodeVar *retty = new UnifNodeVar();
        UnifNode *apty = new UnifNodeConstructor(
            "→", {new UnifNodeConstructor("π", argsty), retty});

        ap->type = retty;
        cerr << "\t-{0} ";
        e->print(cerr, /*withTypes=*/false);
        cerr << "\n";
        unify(apty, ap->rator->type);
        cerr << "\t-{1} ";
        e->print(cerr, /*withTypes=*/true);
        cerr << "\n";
    }
}

struct RaiseUnificationToTypePass {
    static TcType *run(UnifNode *node) {
        node = node->repr();
        set<UnifNodeVar *> vars;
        gatherVars(node, vars);

        map<UnifNodeVar *, TcTypeFA *> var2fa;
        for (UnifNodeVar *v : vars) {
            var2fa[v] = new TcTypeFA("t", nullptr);
        }
        return raise(node, var2fa);
    };
    static TcType *raise(UnifNode *node, map<UnifNodeVar *, TcTypeFA *> m) {
        node = node->repr();
        if (auto v = dynamic_cast<UnifNodeVar *>(node)) {
            auto it = m.find(v);
            assert(it != m.end());
            return new TcTypeVar(it->second);
        }

        auto c = dynamic_cast<UnifNodeConstructor *>(node);
        assert(c);
        vector<TcType *> adults;
        for (UnifNode *child : c->children) {
            adults.push_back(raise(child, m));
        }
        return new TcTypeConstructor(c->name, adults);
    }

    static void gatherVars(UnifNode *node, set<UnifNodeVar *> &vs) {
        node = node->repr();
        if (auto v = dynamic_cast<UnifNodeVar *>(node)) {
            vs.insert(v);
            return;
        }

        auto c = dynamic_cast<UnifNodeConstructor *>(node);
        assert(c);
        for (UnifNode *child : c->children) {
            gatherVars(child, vs);
        }
    }
};

// print an expression with the raised types.
void printExprRaised(Expr *e, int indent = 0) {
    if (auto ident = dynamic_cast<ExprIdent *>(e)) {
        TcType *ty = RaiseUnificationToTypePass::run(ident->type);
        printIndent(cerr, indent);
        e->print(cerr, /*withTypes=*/false);
        ty->print(cerr);
        return;
    }

    auto ap = dynamic_cast<ExprAp *>(e);
    cerr << "(@ ";
    assert(ap);
    printExprRaised(ap->rator);
    for (int i = 0; i < (int)ap->rands.size(); ++i) {
        cerr << "\n";
        printIndent(cerr, indent + 1);
        printExprRaised(ap->rands[i], indent + 1);
    }
    TcType *ty = RaiseUnificationToTypePass::run(ap->type);
    cerr << "\n";
    printIndent(cerr, indent + 1);
    cerr << ":";
    ty->print(cerr);
    cerr << ")";
}

const int BUFSIZE = int(1e9);
char raw_input[BUFSIZE];
int main(int argc, char *argv[]) {
    assert(argc == 2 && "usage: unify <path-to-file>");
    cout << "opening file: |" << argv[1] << "|\n";
    FILE *f = fopen(argv[1], "r");
    assert(f && "unable to open input file");
    fread(raw_input, 1, BUFSIZE, f);
    fclose(f);
    Parser p(argv[1], raw_input);
    Module m = parseTopLevel(p);
    cerr << "\n===parsed module===\n";
    m.print(cerr);
    cerr << "\n";

    generateTypesForDecls(m);
    cerr << "\n===module w/ typed decls===\n";
    m.print(cerr);
    cerr << "\n";

    for (Expr *e : m.es) {
        cerr << "\n==inferring |";
        e->print(cerr, /*withTypes=*/false);
        cerr << "|==\n";
        typeInferExpr(e, m, raw_input);
        cerr << "\n--type after inference:--\n";
        printExprRaised(e);
        cerr << "\n";
    }

    return 0;
}

