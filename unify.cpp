#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <vector>

#include "parser.h"

using namespace std;

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
};

struct SurfaceTypeVar : public SurfaceType {
    Identifier name;
    SurfaceTypeVar(Span span, Identifier name)
        : SurfaceType(span, SurfaceTypeKind::Var), name(name){};
    void print(std::ostream &o) const override { o << name.name; }
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
        o << name.name;
        if (!args.size()) return;
        o << "<";
        for (int i = 0; i < (int)args.size(); ++i) {
            args[i]->print(o);
            if (i + 1 < (int)args.size()) {
                o << ", ";
            }
        }
        o << ">";
    }
};

// decl fnname(t1, t2, ... tn) -> tout
struct FnDecl {
    Span span;
    Identifier name;
    vector<SurfaceType *> argtys;
    SurfaceType *retty;

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
        o << ";";
    }
};

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

enum class ExprKind { Identifier, Ap };
struct Expr {
    Span span;
    ExprKind kind;
    Expr(Span span, ExprKind kind) : span(span), kind(kind){};
    virtual void print(std::ostream &o) const = 0;
};

struct ExprIdent : public Expr {
    Identifier name;
    ExprIdent(Span span, Identifier name)
        : Expr(span, ExprKind::Identifier), name(name){};
    void print(std::ostream &o) const override { o << name; }
};

struct ExprAp : public Expr {
    Expr *rator;
    vector<Expr *> rands;
    ExprAp(Span span, Expr *rator, vector<Expr *> rands)
        : Expr(span, ExprKind::Ap), rator(rator), rands(rands){};

    void print(std::ostream &o) const override {
        if (rator->kind == ExprKind::Identifier) {
            rator->print(o);
        } else {
            o << "(";
            rator->print(o);
            o << ")";
        }
        o << "(";
        for (int i = 0; i < (int)rands.size(); ++i) {
            rands[i]->print(o);
            if (i + 1 < (int)rands.size()) {
                o << ", ";
            }
        }
        o << ")";
    }
};

// expr := apRator apRands
// apRator := '(' expr ')' | ident 
// apRands := '(' expr ',' expr ... ',' expr ')'
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

vector<Expr*> parseApRands(Parser &p) {
    Span open = p.parseOpenRoundBracket();
    if (p.parseOptionalCloseRoundBracket()) {
        return {};
    }

    vector<Expr *> rhs;
    while(1) {
        rhs.push_back(parseExpr(p));
        if (p.parseOptionalComma()) { continue; }
        else { 
            p.parseCloseRoundBracket(open);
            break;
        }
    }
    return rhs;

}
Expr *parseExpr(Parser &p) {
    Loc begin = p.getCurrentLoc();
    Expr *rator = parseApRator(p);
    vector<Expr *> rands = parseApRands(p);
    Loc end = p.getCurrentLoc();
    return new ExprAp(Span(begin, end), rator, rands);
}

struct Module {
    vector<FnDecl *> decls;
    vector<Expr *> es;

    void print(std::ostream &o) const {
        for (FnDecl *decl : decls) {
            decl->print(o);
            o << "\n";
        }

        for (Expr *e : es) {
            e->print(o);
            o << ";\n";
        }
    }
};

Module parseTopLevel(Parser &p) {
    Module m;
    while (!p.eof()) {
        if (optional<Span> sp = p.parseOptionalKeyword("decl")) {
            m.decls.push_back(parseFnDecl(*sp, p));
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

const int BUFSIZE = int(1e9);
char raw[BUFSIZE];
int main(int argc, char *argv[]) {
    assert(argc == 2 && "usage: unify <path-to-file>");
    cout << "opening file: |" << argv[1] << "|\n";
    FILE *f = fopen(argv[1], "r");
    assert(f && "unable to open input file");
    fread(raw, 1, BUFSIZE, f);
    fclose(f);
    Parser p(argv[1], raw);
    Module m = parseTopLevel(p);
    cerr << "\n===parsed module===\n";
    m.print(cerr);
    cerr << "\n";
    return 0;
}

