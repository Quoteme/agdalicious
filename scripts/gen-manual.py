#!/usr/bin/env python3
"""Generate the hover documentation that agdalicious-lsp bundles, from the
sources of the Agda user manual (doc/user-manual in the Agda repository).

    gen-manual.py USER_MANUAL_DIR SOURCES_JSON > manual.json

SOURCES_JSON gives the URLs (see doc-sources.json). The output has one entry
per manual section and per command-line option:

    {"version": "2.8.0",
     "entries": [{"title", "url", "summary", "example", "text"}],
     "terms": {"data": 3, "BUILTIN NATURAL": 17, "--safe": 40, ...},
     "libraries": {...}}

`terms` maps what can be hovered (keywords, symbols, pragmas, built-ins,
primitives, options) to the entry that explains it. Nothing in it is written by
hand: keywords come from the manual's list of keywords, pragmas from its index
of pragmas, built-ins and primitives from the code that declares them.
"""

import json
import re
import sys
import unicodedata
from pathlib import Path

# Where in the manual the lists of things to document are (rst labels).
KEYWORDS_SECTION = "keywords-and-special-symbols"
PRAGMAS_SECTION = "pragmas"

UNDERLINE = re.compile(r"^([=\-~^\"*+#`'.:<>_])\1{2,}\s*$")
LABEL = re.compile(r"^\.\. _([^:`]+):\s*$")
DIRECTIVE = re.compile(r"^(\s*)\.\. ([a-zA-Z-]+)::\s*(.*)$")
ADMONITIONS = {"note", "warning", "tip", "hint", "important", "attention", "caution", "danger", "error", "seealso"}


def make_id(s):
    """The HTML id Sphinx gives a label or title."""
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode().lower()
    return re.sub(r"[^a-z0-9]+", "-", s).strip("-")


def indent(line):
    return len(line) - len(line.lstrip(" "))


def block_end(lines, i, base):
    """End of the block starting after line i: lines blank or indented past `base`."""
    j = i + 1
    while j < len(lines) and (not lines[j].strip() or indent(lines[j]) > base):
        j += 1
    while j > i + 1 and not lines[j - 1].strip():
        j -= 1
    return j


def dedent(block):
    n = min((indent(l) for l in block if l.strip()), default=0)
    block = [l[n:].rstrip() for l in block]
    while block and not block[0]:
        block = block[1:]
    while block and not block[-1]:
        block = block[:-1]
    return block


def title_at(lines, i):
    """(title, style, lines it takes) if a section title starts at line i."""
    l = lines[i]
    if UNDERLINE.match(l) and i + 2 < len(lines) and lines[i + 1].strip() and lines[i + 2].strip() == l.strip():
        return lines[i + 1].strip(), (l[0], True), 3
    if (l.strip() and not l.startswith(" ") and i + 1 < len(lines) and UNDERLINE.match(lines[i + 1])
            and not LABEL.match(l) and not l.startswith("..")
            and len(lines[i + 1].strip()) >= len(l.strip()) // 2):
        return l.strip(), (lines[i + 1][0], False), 2
    return None


class Page:
    def __init__(self, path, url):
        self.url = url
        self.lines = path.read_text(encoding="utf-8").split("\n")
        self.sections = []  # {title, level, labels, start (title line), body, end}
        self.labels = {}  # label -> (section index, anchor)
        styles, pending = [], []
        i = 0
        while i < len(self.lines):
            m = LABEL.match(self.lines[i])
            t = title_at(self.lines, i)
            if m:
                pending.append(m.group(1))
                i += 1
            elif t:
                title, style, n = t
                if style not in styles:
                    styles.append(style)
                s = len(self.sections)
                self.sections.append(dict(title=title, level=styles.index(style), labels=pending, start=i, body=i + n))
                for lab in pending:
                    self.labels[lab] = (s, None)
                pending = []
                i += n
            else:
                if pending and self.lines[i].strip() and self.sections:
                    for lab in pending:  # a label inside a section, e.g. before a paragraph
                        self.labels[lab] = (len(self.sections) - 1, make_id(lab))
                    pending = []
                i += 1
        for k, s in enumerate(self.sections):
            nxt = [t for t in self.sections[k + 1:] if t["level"] <= s["level"]]
            s["end"] = nxt[0]["start"] if nxt else len(self.lines)

    def section_url(self, k):
        s = self.sections[k]
        if s["level"] == 0:
            return self.url
        return self.url + "#" + make_id(s["labels"][0] if s["labels"] else s["title"])

    def innermost(self, line):
        """The innermost section containing a line."""
        best = None
        for k, s in enumerate(self.sections):
            if s["start"] <= line < s["end"] and (best is None or s["level"] > self.sections[best]["level"]):
                best = k
        return best


class Manual:
    def __init__(self, root, base):
        self.base = base
        self.pages = []
        for path in sorted(root.rglob("*.rst")):
            rel = path.relative_to(root).as_posix()
            if rel.startswith(("contribute/", "_")):
                continue
            self.pages.append(Page(path, base + re.sub(r"(\.lagda)?\.rst$", ".html", rel)))
        self.labels = {lab: (p, k, a) for p in self.pages for lab, (k, a) in p.labels.items()}

    def resolve(self, label):
        """(title, url) of an rst label."""
        if label not in self.labels:
            return None
        p, k, anchor = self.labels[label]
        url = p.url + "#" + anchor if anchor else p.section_url(k)
        return p.sections[k]["title"], url

    # -- rst to markdown ----------------------------------------------------

    def inline(self, s):
        def ref(m):
            text, label = m.group(1), m.group(2)
            if label is None:
                label, text = text, None
            r = self.resolve(label.strip())
            text = (text or (r[0] if r else label)).strip()
            return f"[{text}]({r[1]})" if r else text

        codes = []  # code spans, kept out of the way of the other rules

        def code(m):
            codes.append(m.group(1))
            return f"\0{len(codes) - 1}\0"

        s = re.sub(r":ref:`([^`<]+?)(?:\s*<([^`>]+)>)?`", ref, s, flags=re.S)
        s = re.sub(r":(?:[a-z]+:)+`([^`]+)`", code, s)
        s = re.sub(r"``(.+?)``", code, s, flags=re.S)
        s = re.sub(r"`([^`<]+?)\s*<([^`>]+)>`__?", r"[\1](\2)", s, flags=re.S)
        s = re.sub(r"`([^`]+)`_+", r"\1", s)
        s = re.sub(r"`([^`]+)`", r"*\1*", s)
        s = re.sub(r"\0(\d+)\0", lambda m: "`" + codes[int(m.group(1))] + "`", s)
        return s.replace("\\ ", "")

    def markdown(self, lines):
        out = []
        i = 0
        while i < len(lines):
            l = lines[i]
            t = title_at(lines, i)
            d = DIRECTIVE.match(l)
            if not l.strip():
                out.append("")
                i += 1
            elif LABEL.match(l):
                i += 1
            elif t:
                out += ["#### " + self.inline(t[0]), ""]
                i += t[2]
            elif d:
                base, name, arg = indent(l), d.group(2), d.group(3)
                j = block_end(lines, i, base)
                body = dedent(lines[i + 1:j])
                if name == "code-block":
                    out += ["```" + (arg or "agda")] + [b.rstrip() for b in body if True] + ["```"]
                elif name in ADMONITIONS:
                    inner = self.markdown(([arg] if arg else []) + body)
                    out += ["> **" + name.capitalize() + "**"] + ["> " + x if x else ">" for x in inner]
                elif name == "option":
                    out += ["**" + self.inline("``" + arg + "``") + "**", ""] + self.markdown(body)
                elif name in ("versionadded", "versionchanged"):
                    out += [f"*{'Added' if name == 'versionadded' else 'Changed'} in version {arg}.*"] + self.markdown(body)
                i = j  # other directives (index, toctree, comments, hidden code, ...) are left out
            elif l.startswith(".."):
                i = block_end(lines, i, indent(l))  # a comment, or hidden code in literate Agda
            elif l.strip() == "::":
                j = block_end(lines, i, indent(l))
                out += ["```agda"] + [b.rstrip() for b in dedent(lines[i + 1:j])] + ["```"]
                i = j
            else:
                # a paragraph, then maybe a literal block (::) or a definition (indented)
                j = i
                while j < len(lines) and lines[j].strip() and indent(lines[j]) == indent(l) \
                        and not DIRECTIVE.match(lines[j]) and not title_at(lines, j):
                    j += 1
                para = "\n".join(x.strip() for x in lines[i:j])
                literal = para.endswith("::")
                if literal:  # "text::" is "text:", "text ::" and "::" are nothing
                    para = re.sub(r"\s+::$", "", para) if re.search(r"(^|\s)::$", para) else para[:-1]
                if j < len(lines) and lines[j].strip() and indent(lines[j]) > indent(l) and not literal:
                    para = "**" + para + "**"  # a definition-list term
                if para:
                    out.append(self.inline(para))
                if literal:
                    k = j
                    while k < len(lines) and not lines[k].strip():
                        k += 1
                    end = block_end(lines, k - 1, indent(l)) if k < len(lines) else k
                    out += ["", "```agda"] + [b.rstrip() for b in dedent(lines[k:end])] + ["```"]
                    j = end
                elif j < len(lines) and lines[j].strip() and indent(lines[j]) > indent(l):
                    end = block_end(lines, j - 1, indent(l))
                    out += self.markdown(dedent(lines[j:end]))
                    j = end
                i = j
        return out

    # -- entries ---------------------------------------------------------------

    def entry(self, title, url, lines, limit):
        md = self.markdown(lines)
        text = re.sub(r"\n{3,}", "\n\n", "\n".join(md)).strip()
        paras = [p for p in text.split("\n\n") if p and not p.startswith(("```", "####", ">", "*Added", "*Changed"))]
        code = re.search(r"```agda\n(.*?)\n```", text, re.S)
        if len(text) > limit:
            text = text[: text.rfind("\n\n", 0, limit)] + "\n\n…"
        return dict(title=title, url=url, summary=paras[0] if paras else "",
                    example=code.group(1) if code else "", text=text)


def main(root, sources_file):
    sources = json.loads(Path(sources_file).read_text())
    conf = (root / "conf.py").read_text()
    version = re.search(r"^version\s*=\s*['\"]([^'\"]+)", conf, re.M).group(1)
    manual = Manual(root, sources["manual"].format(version=version))
    limit = sources["maxLength"]

    entries, index, terms = [], {}, {}

    def section_entry(p, k):
        if (p.url, k) not in index:
            s = p.sections[k]
            index[(p.url, k)] = len(entries)
            entries.append(manual.entry(s["title"], p.section_url(k), p.lines[s["body"]:s["end"]], limit))
        return index[(p.url, k)]

    def label_entry(label):
        p, k, _ = manual.labels[label]
        return section_entry(p, k)

    # pragmas, from the index of pragmas
    p, k, _ = manual.labels[PRAGMAS_SECTION]
    for m in re.finditer(r":ref:`([A-Z_]+)\s*<([^>]+)>`", "\n".join(p.lines[p.sections[k]["body"]:p.sections[k]["end"]])):
        if m.group(2) in manual.labels:
            terms.setdefault(m.group(1), label_entry(m.group(2)))

    # options: one entry each
    for p in manual.pages:
        for i, l in enumerate(p.lines):
            d = DIRECTIVE.match(l)
            if d and d.group(2) == "option":
                names = [re.split(r"[=\s\[{]", o.strip())[0] for o in d.group(3).split(",")]
                url = p.url + "#cmdoption-" + names[0].lstrip("-")
                body = dedent(p.lines[i + 1:block_end(p.lines, i, indent(l))])
                entries.append(manual.entry(d.group(3), url, body, limit))
                for n in names:
                    terms.setdefault(n, len(entries) - 1)

    # built-ins and primitives, from the code that declares them
    for p in manual.pages:
        for i, l in enumerate(p.lines):
            k = p.innermost(i)
            if k is None:
                continue
            for m in re.finditer(r"\{-#\s*BUILTIN\s+([A-Z0-9_]+)", l):
                terms.setdefault("BUILTIN " + m.group(1), section_entry(p, k))
            if re.match(r"^\s+primitive\s*$", l):
                for x in p.lines[i + 1:block_end(p.lines, i, indent(l))]:
                    m = re.match(r"^\s+(prim\w+)\s*:", x)
                    if m:
                        terms.setdefault(m.group(1), section_entry(p, k))

    # keywords and symbols, from the list of keywords
    p, k, _ = manual.labels[KEYWORDS_SECTION]
    s = p.sections[k]
    body = p.lines[s["body"]:s["end"]]
    plain = []
    for i, l in enumerate(body):
        if l.strip() == "keywords":
            block = "\n".join(body[i + 1:block_end(body, i, indent(l))])
            for m in re.finditer(r":ref:`(\S+)\s*<([^>]+)>`|``(\S+?)``", block):
                if m.group(1):
                    terms.setdefault(m.group(1), label_entry(m.group(2)))
                else:
                    plain.append(m.group(3))

    # A keyword without a link: the section of the language reference named
    # after it (a page, or a section of exactly that name, else the top-most),
    # else the page there that mentions it most (other than the list of
    # keywords), else one elsewhere named after it. Symbols are only explained
    # by the list of keywords. `overrides` in SOURCES_JSON settles the rest.
    def token(kw):
        return r"(?<![\w-])" + re.escape(kw) + r"(?![\w-])"

    def mentions(p, s, kw):
        """How often a section has the keyword in a code literal, else in its code."""
        text = "\n".join(p.lines[s["body"]:s["end"]])
        return (sum(1 for m in re.finditer(r"``(.+?)``", text, re.S) if re.search(token(kw), m.group(1))),
                len(re.findall(token(kw), "\n".join(l for l in p.lines[s["body"]:s["end"]] if l.startswith(" ")))))

    kp, kk, _ = manual.labels[KEYWORDS_SECTION]
    kstart = kp.sections[kk]["start"]
    preferred = [p for p in manual.pages if p.url.startswith(tuple(manual.base + d for d in sources["preferred"]))]
    for kw, label in sources["overrides"].items():
        terms[kw] = label_entry(label)
    for kw in plain:
        if kw in terms:
            continue
        if not re.search(r"[A-Za-z0-9]", kw):
            terms[kw] = section_entry(kp, kk)
            continue
        word = re.compile(r"(?<![\w-])" + re.escape(kw) + r"(s|es)?(?!\w)", re.I)
        exact = re.compile(r"^(``)?" + re.escape(kw) + r"(s|es)?(``)?$", re.I)
        named = sorted(((p not in preferred, not (exact.match(s["title"]) or s["level"] == 0), s["level"], len(s["title"]), p, k)
                        for p in manual.pages for k, s in enumerate(p.sections)
                        if len(kw) > 2 and word.search(s["title"])
                        and (s["level"] == 0 or exact.match(s["title"]) or mentions(p, s, kw)[0])), key=lambda x: x[:4])
        counts = [(mentions(p, s, kw), -s["level"], p, k) for p in preferred
                  for k, s in enumerate(p.sections) if not (p is kp and s["start"] <= kstart < s["end"])]
        counts = [c for c in counts if c[0] > (0, 0)]
        if named and named[0][0] is False:
            *_, p, k = named[0]
        elif counts:
            *_, p, k = max(counts, key=lambda x: x[:2])
        elif named:
            *_, p, k = named[0]
        else:
            continue
        terms[kw] = section_entry(p, k)

    json.dump(dict(version=version, entries=entries, terms=terms, libraries=sources["libraries"]),
              sys.stdout, ensure_ascii=False, indent=None)


if __name__ == "__main__":
    main(Path(sys.argv[1]), sys.argv[2])
