#pragma once
#include <assert.h>
#include <string.h>

#include <iostream>
#include <optional>
#include <string>
#include <utility>
#include <vector>

struct Span;
struct Loc {
    const char *filename;
    long long si, line, col;

    Loc(const char *filename, long long si, long long line, long long col)
        : filename(filename), si(si), line(line), col(col){};

    Loc nextline() const { return Loc(filename, si + 1, line + 1, 1); }

    Loc nextc(char c) const {
        if (c == '\n') {
            return nextline();
        } else {
            return nextcol();
        }
    }

    Loc nextstr(const std::string &s) const;

    Loc prev(char c) const {
        if (c == '\n') {
            assert(false && "don't know how to walk back newline");
        } else {
            return prevcol();
        }
    }

    Loc prev(const char *s) const {
        Loc l = *this;
        for (int i = strlen(s) - 1; i >= 0; --i) {
            l = l.prev(s[i]);
        }
        return l;
    }

    bool operator==(const Loc &other) const {
        return si == other.si && line == other.line && col == other.col;
    }

    bool operator!=(const Loc &other) const { return !(*this == other); }

    // move the current location to the location Next, returning
    // the distance spanned.
    Span moveMut(Loc next);

    std::string to_str() const {
        return ":" + std::to_string(line) + ":" + std::to_string(col);
    }

   private:
    Loc nextcol() const { return Loc(filename, si + 1, line, col + 1); }

    Loc prevcol() const {
        assert(col - 1 >= 1);
        return Loc(filename, si - 1, line, col - 1);
    }
};

std::ostream &operator<<(std::ostream &o, const Loc &l) {
    return o << ":" << l.line << ":" << l.col;
}

// half open [...)
// substr := str[span.begin...span.end-1];
struct Span {
    Loc begin, end;

    Span(Loc begin, Loc end) : begin(begin), end(end) {
        assert(end.si >= begin.si);
        assert(!strcmp(begin.filename, end.filename));
    };

    long long nchars() const { return end.si - begin.si; }

    Span extendRight(Span rightward) const {
        assert(this->end.si <= rightward.begin.si);
        return Span(this->begin, rightward.end);
    }

    Span extendRight(Loc rightward) const {
        assert(this->end.si <= rightward.si);
        return Span(this->begin, rightward);
    }

    std::string to_str() const { return begin.to_str() + " - " + end.to_str(); }
};

Span Loc::moveMut(Loc next) {
    assert(next.si >= this->si);
    Span s(*this, next);
    *this = next;
    return s;
}

std::ostream &operator<<(std::ostream &o, const Span &s) {
    return o << s.begin << " - " << s.end;
}

std::string vprintfspan(Span span, const char *raw_input, const char *fmt,
                        va_list args) {
    const int CONTEXTLEN = 25;
    // const int LINELEN = 80;

    char *outstr = nullptr;
    vasprintf(&outstr, fmt, args);
    assert(outstr);

    if (span.begin.line == span.end.line) {
        std::string errstr, cursorstr;
        const long long nchars_back = ({
            long long i = 0;
            while (1) {
                if (span.begin.si - i == 0) {
                    break;
                }
                if (raw_input[span.begin.si - i] == '\n') {
                    i--;
                    break;
                }
                if (i > CONTEXTLEN) {
                    break;
                }
                i++;
            }
            i;
        });

        const long long nchars_fwd = ({
            long long i = 0;
            while (1) {
                if (raw_input[span.begin.si + i] == 0) {
                    break;
                }
                if (raw_input[span.begin.si + i] == '\n') {
                    i--;
                    break;
                }
                if (i > CONTEXTLEN) {
                    break;
                }
                i++;
            }
            i;
        });

        if (nchars_back > CONTEXTLEN) {
            cursorstr += "   ";
            errstr += "...";
        }

        errstr += std::string(raw_input + span.begin.si - nchars_back,
                              raw_input + span.end.si + nchars_fwd);
        cursorstr += std::string(nchars_back, ' ');
        cursorstr += std::string(span.end.si - span.begin.si + 1, '^');
        cursorstr += std::string(nchars_fwd, ' ');

        if (nchars_fwd > CONTEXTLEN) {
            cursorstr += "   ";
            errstr += "...";
        }

        return "\n==\n" + std::string(outstr) + "\n" + span.begin.filename +
               span.to_str() + "\n" + errstr + "\n" + cursorstr + "\n==\n";
    } else {
        // multi-line error.
        std::string errstr = "";
        for (int i = span.begin.si; i != span.end.si; ++i) {
            errstr += raw_input[i];
            if (raw_input[i] == '\n') {
                errstr += '>';
            }
        }

        return "\n==\n" + std::string(outstr) + "\n" + span.begin.filename +
               span.to_str() + "\n" + errstr + "\n==\n";
    }
}

bool isWhitespace(char c) { return c == ' ' || c == '\n' || c == '\t'; }
bool isReservedSigil(char c) {
    return c == '(' || c == ')' || c == '{' || c == '}' || c == ',' ||
           c == ';' || c == '[' || c == ']' || c == ':' || c == '-' ||
           c == '*' || c == '+' || c == '/' || c == '!' || c == '<' || 
           c == '>';
}

std::string printfspan(Span span, const char *raw_input, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    std::string s = vprintfspan(span, raw_input, fmt, args);
    va_end(args);
    return s;
}

struct ParseError {
    std::string errmsg;
    Span span;

    ParseError(Span span, std::string errmsg) : errmsg(errmsg), span(span){};
};

std::string substring(const std::string &s, Span span) {
    assert(span.end.si - span.begin.si >= 0);
    std::string sub = s.substr(span.begin.si, span.end.si - span.begin.si);
    return sub;
}

struct Identifier {
    const Span span;
    const std::string name;
    Identifier(const Identifier &other) = default;
    Identifier(Span span, std::string name) : span(span), name(name){};

    Identifier operator=(const Identifier &other) { return Identifier(other); }

    void print(std::ostream &f) const { f << name; }
};

std::ostream &operator<<(std::ostream &o, Identifier i) {
    i.print(o);
    return o;
}

struct Parser {
    Parser(const char *filename, const char *raw)
        : s(raw), l(filename, 0, 1, 1){};

    void parseOpenCurly() { parseSigil("{"); }
    void parseCloseCurly() { parseSigil("}"); }
    void parseFatArrow() { parseSigil("=>"); }
    bool parseOptionalCloseCurly() { return bool(parseOptionalSigil("}")); }
    Span parseOpenRoundBracket() { return parseSigil("("); }
    std::optional<Span> parseOptionalOpenCurly() {
        return parseOptionalSigil("{");
    }

    std::optional<Span> parseOptionalOpenRoundBracket() {
        return parseOptionalSigil("(");
    }

    Span parseCloseRoundBracket(Span open) {
        return parseMatchingSigil(open, ")");
    }

    bool parseOptionalCloseRoundBracket() {
        return bool(parseOptionalSigil(")"));
    }
    void parseColon() { parseSigil(":"); }
    void parseEqual() { parseSigil("="); }

    bool parseOptionalComma() { return bool(parseOptionalSigil(",")); }

    void parseComma() { parseSigil(","); }
    void parseSemicolon() { parseSigil(";"); }
    bool parseOptionalSemicolon() { return bool(parseOptionalSigil(";")); }
    void parseThinArrow() { parseSigil("->"); }

    std::pair<Span, long long> parseInteger() {
        std::optional<std::pair<Span, long long>> out = parseOptionalInteger();
        if (!out) {
            this->addErr(ParseError(Span(l, l), "unble to find integer"));
            exit(1);
        }
        return *out;
    }

    // [-][0-9]+
    std::optional<std::pair<Span, long long>> parseOptionalInteger() {
        eatWhitespace();
        bool negate = false;
        std::optional<char> ccur;  // peeking character
        Loc lcur = l;

        ccur = this->at(lcur);
        if (!ccur) {
            return {};
        }
        if (*ccur == '-') {
            negate = true;
            lcur = lcur.nextc(*ccur);
        }

        long long number = 0;
        while (1) {
            ccur = this->at(lcur);
            if (!ccur) {
                break;
            }
            if (!isdigit(*ccur)) {
                break;
            }
            number = number * 10 + (*ccur - '0');
            lcur = lcur.nextc(*ccur);
        }
        Span span = l.moveMut(lcur);
        if (span.nchars() == 0) {
            return {};
        }
        if (negate) {
            number *= -1;
        }

        return {{span, number}};
    }

    Span parseSigil(const std::string sigil) {
        std::optional<Span> span = parseOptionalSigil(sigil);
        if (span) {
            return *span;
        }

        addErr(ParseError(Span(l, l), "expected sigil> |" + sigil + "|"));
        exit(1);
    }

    Span parseMatchingSigil(Span open, const std::string sigil) {
        std::optional<Span> span = parseOptionalSigil(sigil);
        if (span) {
            return *span;
        }

        addErr(ParseError(Span(l, l), "expected sigil: |" + sigil + "|"));
        exit(1);
    }

    // difference is that a sigil needs no whitespace after it, unlike
    // a keyword.
    std::optional<Span> parseOptionalSigil(const std::string sigil) {
        std::optional<char> ccur;
        eatWhitespace();
        Loc lcur = l;
        // <sigil>

        for (long long i = 0; i < (int)sigil.size(); ++i) {
            ccur = this->at(lcur);
            if (!ccur) {
                return {};
            }
            //            cerr << "-sigil: |" << ccur << "|\n";
            if (*ccur != sigil[i]) {
                return {};
            }
            lcur = lcur.nextc(*ccur);
        }

        Span span = l.moveMut(lcur);
        return span;
    }

    Identifier parseIdentifier() {
        std::optional<Identifier> ms = parseOptionalIdentifier();
        if (ms.has_value()) {
            return *ms;
        }
        addErr(ParseError(Span(l, l), "expected identifier"));
        exit(1);
    }

    std::optional<Identifier> parseOptionalIdentifier() {
        eatWhitespace();
        Loc lcur = l;

        std::optional<char> fst = this->at(lcur);
        if (!fst) {
            return {};
        }
        if (!(isalpha(*fst) || *fst == '_')) {
            return {};
        }
        lcur = lcur.nextc(*fst);

        while (1) {
            std::optional<char> cchar = this->at(lcur);
            if (!cchar) {
                return {};
            }
            if (isWhitespace(*cchar) || isReservedSigil(*cchar)) {
                break;
            }
            lcur = lcur.nextc(s[lcur.si]);
        }

        const Span span = l.moveMut(lcur);
        return Identifier(span, substring(s, span));
    }

    std::optional<Span> parseOptionalKeyword(const std::string keyword) {
        eatWhitespace();
        // <keyword><non-alpha-numeric>
        Loc lcur = l;
        for (int i = 0; i < (int)keyword.size(); ++i) {
            std::optional<char> c = this->at(lcur);
            if (!c) {
                return {};
            }
            if (c != keyword[i]) {
                return {};
            }
            lcur = lcur.nextc(*c);
        }
        std::optional<char> c = this->at(lcur);
        if (!c) {
            return {};
        }
        if (isalnum(*c)) {
            return {};
        }

        return l.moveMut(lcur);
    };

    Span parseKeyword(const std::string keyword) {
        std::optional<Span> ms = parseOptionalKeyword(keyword);
        if (ms) {
            return *ms;
        }
        addErr(ParseError(Span(l, l), "expected |" + keyword + "|"));
        exit(1);
    }

    void addNote(ParseError e) {
        std::cerr << printfspan(e.span,  this->s.c_str(), e.errmsg.c_str());
        std::cerr.flush();
        return;
    }

    void addErr(ParseError e) {
        std::cerr << printfspan(e.span,  this->s.c_str(), e.errmsg.c_str());
        std::cerr.flush();
        assert(false && "unable to parse");
        // errs.push_back(e);
        // printfspan(e.span, s.c_str(), e.errmsg.c_str());
    }

    void addErrAtCurrentLoc(std::string err) {
        addErr(ParseError(Span(l, l), err));
    }

    bool eof() {
        eatWhitespace();
        return l.si == (long long)s.size();
    }

    // eat till newline
    void eatTillNewline() {
        while (1) {
            std::optional<char> c = this->at(l);
            if (!c) {
                return;
            }
            l = l.nextc(*c);
            if (c == '\n') {
                return;
            }
        }
    }

    Loc getCurrentLoc() {
        eatWhitespace();
        return l;
    }

    // return the rest of the string for debugging
    const char *debugRestOfFile() { return s.c_str() + l.si; }

   private:
    const std::string s;
    Loc l;
    std::vector<ParseError> errs;

    std::optional<char> at(Loc loc) {
        if (loc.si >= (int)s.size()) {
            return std::optional<char>();
        }
        return s[loc.si];
    }

    void eatWhitespace() {
        while (1) {
            std::optional<char> ccur = this->at(l);
            if (!ccur) {
                return;
            }
            if (!isWhitespace(*ccur)) {
                return;
            }
            l = l.nextc(*ccur);
        }
    }
};


// utility to print a number as a collection of letters.
char *getPronouncableNum(size_t N) {
  const char *cs = "bcdfghjklmnpqrstvwxzy";
  const char *vs = "iou";

  size_t ncs = strlen(cs);
  size_t nvs = strlen(vs);

  char buf[1024];
  char *out = buf;
  int i = 0;
  while (N > 0) {
    const size_t icur = N % (ncs * nvs);
    *out++ = cs[icur % ncs];
    *out++ = vs[(icur / ncs) % nvs];
    N /= ncs * nvs;
    if (N > 0 && !(++i % 2)) {
      *out++ = '-';
    }
  }
  *out = 0;
  return strdup(buf);
};
