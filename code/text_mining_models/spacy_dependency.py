# Use spaCy dependency parsing to extract subject-relation-object triples.
#
# Handles the relation patterns common in GWAS cohort sentences, e.g.:
#   "From 2009 to 2012, samples for the GWAS were obtained from 224 Japanese
#    CHC patients who were treated at 11 multi-center hospitals throughout Japan."
#
# Unlike a naive SVO extractor, this:
#   - handles PASSIVE voice (nsubjpass / agent "by ...")
#   - reaches PREPOSITIONAL objects (verb -> prep -> pobj), which is where most
#     of the useful nouns live ("from <patients>", "at <hospitals>", "in <Japan>")
#   - returns FULL noun-phrase spans ("224 Japanese CHC patients"), not single tokens
#   - walks relative-clause verbs ("who were treated at ...")
#
# Works with either a standard spaCy English model (en_core_web_sm) or a
# scispacy biomedical model (en_core_sci_sm / _md / _lg). Install (one-off):
#   pip install scispacy
#   pip install https://s3-us-west-2.amazonaws.com/ai2-s2-scispacy/releases/v0.5.4/en_core_sci_sm-0.5.4.tar.gz
# Note: the en_ner_* scispacy models are NER-only and have no parser -- use an
# en_core_sci_* model for dependency parsing.
#
# IMPORTANT: the two model families use DIFFERENT dependency-label schemes:
#   * en_core_web_* (ClearNLP): nsubjpass, dobj, prep + pobj, relcl
#   * en_core_sci_* (Universal Dependencies): nsubj:pass, obj, obl/nmod + case,
#                                             acl:relcl  (no prep/pobj at all)
# The extractor below normalises both so it works with either model. Run with
# DEBUG=1 to dump per-token pos/dep and see which scheme your model emits:
#   DEBUG=1 python3 code/text_mining_models/spacy_dependency.py
import csv
import json
import os
import sys
import spacy

try:
    # Used to locate test_sentences.jsonl relative to the project root.
    from pyprojroot import here
except ImportError:
    here = None

# Change this to "en_core_web_sm" to fall back to the general-English model.
MODEL = "en_core_sci_lg"

# Base labels (UD sublabels like "nsubj:pass" are matched on the part before ":").
SUBJECT_DEPS = {"nsubj", "nsubjpass", "csubj", "csubjpass"}
# Direct (non-prepositional) object roles attached straight to the verb.
# Includes both ClearNLP (dobj/dative/attr/oprd) and UD (obj/iobj).
DIRECT_OBJECT_DEPS = {"dobj", "obj", "dative", "iobj", "attr", "oprd"}
# Oblique/prepositional noun attached straight to the verb in the UD scheme.
OBLIQUE_DEPS = {"obl", "nmod"}
RELCL_DEPS = {"relcl", "acl"}
REL_PRONOUNS = {"who", "whom", "which", "that", "whose"}

# Drop objects that are purely a number/date (e.g. the "From 2009 to 2012" in
# "obtained from 224 ... patients"), which are temporal context, not cohorts.
DROP_NUMERIC_OBJECTS = False


def _base(dep):
    """Strip UD sub-labels, e.g. 'nsubj:pass' -> 'nsubj', 'acl:relcl' -> 'acl'."""
    return dep.split(":", 1)[0]


def np_span(token):
    """Return the full noun-phrase text for a token (its subtree, in word order)."""
    subtree = list(token.subtree)
    start = min(t.i for t in subtree)
    end = max(t.i for t in subtree)
    return token.doc[start:end + 1].text


def head_np(token):
    """Noun-phrase *Span* around `token`, without trailing relative clauses.

    Keeps immediate determiners/adjectives/compounds/numbers so we get
    '224 Japanese CHC patients' but not the attached 'who were treated ...'.
    Returns a spaCy Span so callers can align it against doc.ents.
    """
    keep_deps = {"det",
                 "amod",
                 "compound",
                 "nummod",
                 "nmod",
                 "poss",
                 "quantmod",
                 "advmod"}
    toks = [token] + [c for c in token.children if c.dep_ in keep_deps]
    start = min(t.i for t in toks)
    end = max(t.i for t in toks)
    return token.doc[start:end + 1]


def head_np_span(token):
    """Text of head_np (kept for backwards compatibility)."""
    return head_np(token).text


def relation_spans(doc):
    """Yield (subject_span, relation, object_span) for every verb in the doc.

    subject_span may be None; the others are spaCy Spans. `relation` is the
    verb lemma, with the preposition appended for prepositional objects
    (e.g. 'obtain:from', 'treat:at'). Spans (rather than text) let callers
    align each argument against doc.ents -- see relations_with_entities().
    """
    for sent in doc.sents:
        for verb in (t for t in sent if t.pos_ in ("VERB", "AUX")):
            in_relcl = _base(verb.dep_) in RELCL_DEPS

            subjects = []
            for c in verb.children:
                if _base(c.dep_) not in SUBJECT_DEPS:
                    continue
                # Resolve relative pronouns ("who"/"which"/"that") to the noun
                # the clause modifies, so we get "224 ... patients", not "who".
                if in_relcl and c.lemma_.lower() in REL_PRONOUNS:
                    subjects.append(head_np(verb.head))
                else:
                    subjects.append(head_np(c))

            # Passive agent: "... obtained by <X>" -> subject is the agent.
            for c in verb.children:
                if c.dep_ == "agent":
                    subjects += [head_np(p) for p in c.children if p.dep_ == "pobj"]

            # Direct objects attached straight to the verb (both schemes).
            objects = [(verb.lemma_, c) for c in verb.children
                       if _base(c.dep_) in DIRECT_OBJECT_DEPS]

            # ClearNLP prepositional objects: verb -> prep -> pobj.
            for prep in (c for c in verb.children if c.dep_ == "prep"):
                rel = f"{verb.lemma_}:{prep.text.lower()}"
                objects += [(rel, p) for p in prep.children if p.dep_ == "pobj"]

            # UD oblique objects: verb -> obl/nmod (the noun itself), with the
            # preposition attached to that noun as a 'case' child.
            for c in verb.children:
                if _base(c.dep_) in OBLIQUE_DEPS:
                    case = [t.text.lower() for t in c.children if t.dep_ == "case"]
                    rel = f"{verb.lemma_}:{case[0]}" if case else verb.lemma_
                    objects.append((rel, c))

            # Fallback for relative clauses whose subject pronoun wasn't attached
            # to this verb: use the noun the clause modifies.
            if not subjects and in_relcl:
                subjects = [head_np(verb.head)]

            # Drop temporal/numeric objects ("From 2009 to 2012"), keeping cohorts.
            objects = [(rel, head_np(tok)) for rel, tok in objects
                       if not (DROP_NUMERIC_OBJECTS and tok.pos_ == "NUM")]

            for subj in (subjects or [None]):
                for rel, obj in objects:
                    yield subj, rel, obj


def extract_relations(doc):
    """(subject_text, relation, object_text) triples -- text-only convenience."""
    return [(s.text if s is not None else None, rel, o.text)
            for s, rel, o in relation_spans(doc)]


# ---------------------------------------------------------------------------
# NER integration
# ---------------------------------------------------------------------------
# The parser gives the relation skeleton; NER labels the entities *inside* each
# argument span. We link them by span overlap: an entity belongs to an argument
# if their token ranges intersect. Entities can come from:
#   (a) the same model's doc.ents (en_core_sci_* gives untyped "ENTITY"),
#   (b) a typed scispacy NER model (en_ner_bc5cdr_md -> DISEASE/CHEMICAL,
#       en_ner_bionlp13cg_md -> genes, cell types, ...), projected on with
#       project_entities(), or
#   (c) your own PubMedBERT tags converted to char spans (see spans_from_labels).

def entities_in_span(span, ents):
    """Entities (Spans) overlapping `span`, as [(text, label), ...]."""
    if span is None:
        return []
    return [(e.text, e.label_) for e in ents
            if e.start < span.end and e.end > span.start]


def project_entities(parse_doc, ner_doc):
    """Re-project ents from a separate NER model's doc onto the parsed doc.

    Use when your parser model and NER model are different (e.g. en_core_sci_sm
    for parsing + en_ner_bc5cdr_md for typed entities). Matches by char offset.
    """
    ents = []
    for e in ner_doc.ents:
        span = parse_doc.char_span(e.start_char, e.end_char,
                                   label=e.label_, alignment_mode="expand")
        if span is not None:
            ents.append(span)
    return ents


def spans_from_labels(doc, char_spans):
    """Build entity Spans from external (start_char, end_char, label) tuples,
    e.g. doccano gold annotations or PubMedBERT predictions."""
    ents = []
    for start_char, end_char, label in char_spans:
        span = doc.char_span(start_char, end_char, label=label,
                             alignment_mode="expand")
        if span is not None:
            ents.append(span)
    return ents


def load_doccano(path):
    """Yield (record_id, text, char_spans) from a doccano JSONL export.

    Each line is a JSON object with a `text` field and a span list under
    `label` (or `labels`/`entities`, depending on the export), where each span
    is [start_char, end_char, label]. The returned char_spans feed straight into
    spans_from_labels() to act as the NER layer for relations_with_entities().
    """
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            rec = json.loads(line)
            raw = rec.get("label") or rec.get("labels") or rec.get("entities") or []
            spans = [(s[0], s[1], s[2]) for s in raw]
            yield rec.get("id"), rec["text"], spans


def relations_with_entities(doc, ents=None):
    """Relations enriched with the NER entities found in each argument.

    `ents` is a list of entity Spans; defaults to doc.ents. Returns dicts with
    subject/object text plus their overlapping (entity_text, label) lists.
    """
    if ents is None:
        ents = list(doc.ents)
    rows = []
    for subj, rel, obj in relation_spans(doc):
        rows.append({
            "subject": subj.text if subj is not None else None,
            "subject_entities": entities_in_span(subj, ents),
            "relation": rel,
            "object": obj.text,
            "object_entities": entities_in_span(obj, ents),
        })
    return rows


# ---------------------------------------------------------------------------
# Containment between numeric counts (N is part of / subset of M)
# ---------------------------------------------------------------------------
# Containment is not a dependency label -- it is inferred from where each number
# attaches (every count attaches to a noun via nummod) plus a few constructions:
#   * partitive   "Of the 1,077 X, 915 X had ..."   -> 915 subset 1,077
#   * apposition  "915 X (539 Y and 376 Z)"          -> 539, 376 subset 915
#   * relcl       "1,373 X, which included 345 ..."  -> 345 subset 1,373
# A numeric check (child <= parent) prunes inversions and lets the walk skip a
# too-small enclosing count (e.g. 1,028 is not part of the nearer 345 samples,
# so we keep climbing to 1,373).

def _to_int(text):
    """'1,077' -> 1077; non-numeric -> None."""
    try:
        return int(str(text).replace(",", "").strip())
    except (ValueError, AttributeError):
        return None


# Prepositions that introduce a containing "whole" ("915 ... from 1,077 ...").
WHOLE_PREPS = {"of", "from", "among", "amongst", "within", "out"}
# Pronouns in "N of whom / of which / of them" partitive breakdowns.
PARTITIVE_PRONOUNS = {"whom", "which", "them", "these", "those"}
# Dependency links (besides plain conj) that a coordinated sibling may carry,
# depending on the model. We only treat them as coordination when a coordinating
# conjunction (cc, i.e. 'and'/'or') is present in the local structure.
COORD_DEPS = {"conj", "dep", "appos", "flat"}


def _has_cc(token):
    """True if `token` has a coordinating-conjunction child ('and' / 'or')."""
    return any(_base(c.dep_) == "cc" for c in token.children)


def _is_coord_link(tok):
    """Is `tok` attached to its head as a coordinated sibling? Robust to models
    that label later conjuncts 'conj' OR 'dep'/'appos' (with a cc present)."""
    base = _base(tok.dep_)
    if base == "conj":
        return True
    return base in COORD_DEPS and (_has_cc(tok) or _has_cc(tok.head))


def _coord_lead(noun):
    """Climb to the lead conjunct of a coordination ('A, B and C' -> A)."""
    cur, seen = noun, set()
    while cur.i not in seen and cur.head is not cur:
        seen.add(cur.i)
        if _is_coord_link(cur):
            cur = cur.head
        else:
            break
    return cur


def _coord_group(noun):
    """Return (lead, set_of_token_indices) for noun's coordination group, so
    siblings joined by 'and'/commas are never treated as each other's parent."""
    lead = _coord_lead(noun)
    group, stack = {lead.i}, [lead]
    while stack:
        t = stack.pop()
        for c in t.children:
            if _is_coord_link(c) and c.i not in group:
                group.add(c.i)
                stack.append(c)
    return lead, group


def _whole_in_oblique(verb, group, noun_to_count, val):
    """Find a containing count inside a from/of/among oblique phrase of `verb`
    (e.g. '... chose 50 ... from the recruited group of 411 women' -> 411)."""
    best = None
    for c in verb.children:
        if _base(c.dep_) not in OBLIQUE_DEPS:
            continue
        if not any(t.dep_ == "case" and t.lemma_.lower() in WHOLE_PREPS
                   for t in c.children):
            continue
        for t in c.subtree:                 # the count may be nested in the phrase
            if t.i in noun_to_count and t.i not in group:
                cand = noun_to_count[t.i]
                if cand[1] >= val and (best is None or cand[1] > best[1]):
                    best = cand
    return best


def _relcl_antecedent(token):
    """If `token` is inside an acl:relcl, return the noun the clause modifies."""
    for anc in token.ancestors:
        if _base(anc.dep_) in RELCL_DEPS:
            return anc.head
    return None


def number_containment(doc, number_ents=None):
    """Infer (child_n is part of parent_n) relations between counts in `doc`.

    Returns dicts: {child, child_n, parent, parent_n, rule}. If number_ents is
    given (e.g. doccano 'N' spans), only those numbers are considered and their
    full text is used for the value -- more reliable than single tokens for
    comma-grouped numbers like '1,077'. Heuristic: verify against a DEBUG parse.
    """
    # counts: list of (label_obj, num_token, value, quantified_noun_token)
    counts = []
    noun_to_count = {}
    def quantified_noun(num_tok):
        """The noun a number modifies. In '989 and 767 samples' the parser hangs
        767 off the number 989, so climb through any numeric head to the noun."""
        noun = num_tok.head
        seen = set()
        while noun is not noun.head and noun.i not in seen and (
                noun.pos_ == "NUM" or noun.like_num):
            seen.add(noun.i)
            noun = noun.head
        return noun

    def add_count(label_obj, num_tok, val):
        noun = quantified_noun(num_tok)
        counts.append((label_obj, num_tok, val, noun))
        noun_to_count[noun.i] = (label_obj, val, noun)

    if number_ents is not None:
        for e in number_ents:
            if e.label_ != "N":
                continue
            val = _to_int(e.text)
            if val is not None:
                add_count(e, e.root, val)
    else:
        for tok in doc:
            if tok.pos_ != "NUM" and not tok.like_num:
                continue
            val = _to_int(tok.text)
            if val is not None:
                add_count(tok, tok, val)

    # Flag "N of whom / of which / of them" partitives -- these are subset
    # breakdowns of an earlier cohort, regardless of where the parse threads them.
    def _is_of_whom(num_tok):
        for t in num_tok.subtree:
            if t.text.lower() in PARTITIVE_PRONOUNS and any(
                    c.dep_ == "case" and c.lemma_.lower() == "of"
                    for c in t.children):
                return True
        return False

    partitive = {nt.i for (_, nt, _, _) in counts if _is_of_whom(nt)}
    by_pos = sorted(counts, key=lambda c: c[1].i)

    def antecedent_for(num_tok, val):
        """Nearest preceding NON-partitive count large enough to contain `val`
        (so two coordinated 'N of whom' siblings both attach to the cohort)."""
        best = None
        for label_obj, nt, v, nn in by_pos:
            if nt.i >= num_tok.i:
                break
            if nt.i in partitive or v < val:
                continue
            best = (label_obj, v)
        return best

    results = []

    def emit(child, child_n, parent, parent_n, rule):
        results.append({"child": child.text, "child_n": child_n,
                        "parent": parent.text, "parent_n": parent_n, "rule": rule,
                        "sentence": parent.sent.text})

    for child, num_tok, val, noun in counts:
        # Resolve the parent of the whole coordination so siblings joined by
        # 'and'/commas share a parent and are never each other's parent.
        lead, group = _coord_group(noun)
        matched = False

        # Rule 0: "N of whom / which" partitive -> subset of the earlier cohort.
        if num_tok.i in partitive:
            anc = antecedent_for(num_tok, val)
            if anc is not None:
                emit(child, val, anc[0], anc[1], "of-whom")
                matched = True
        if matched:
            continue

        # Rule 1: subject/object of a verb whose from/of/among oblique phrase
        # holds the containing count. "Of the N1 ..., N2 V" / "V N2 ... from N1".
        if _base(lead.dep_) in ("nsubj", "nsubjpass", "obj", "dobj", "iobj"):
            whole = _whole_in_oblique(lead.head, group, noun_to_count, val)
            if whole is not None:
                emit(child, val, whole[0], whole[1], "partitive")
                matched = True
        if matched:
            continue

        # Rule 2: nearest enclosing count via ancestry (apposition / nesting),
        # skipping coordination siblings and counts too small (numeric check).
        for anc in lead.ancestors:
            if anc.i in group:
                continue
            if anc.i in noun_to_count and anc.i != noun.i:
                p, pval, _ = noun_to_count[anc.i]
                if pval >= val:
                    emit(child, val, p, pval, "nested")
                    matched = True
                    break
        if matched:
            continue

        # Rule 3: relative-clause scope. Numbers inside "..., which ..." belong
        # to the antecedent count (handles deeply nested 1,028 sub 1,373).
        ant = _relcl_antecedent(lead)
        if ant is not None and ant.i in noun_to_count and ant.i not in group:
            p, pval, _ = noun_to_count[ant.i]
            if pval >= val:
                emit(child, val, p, pval, "relcl-scope")

    return results


if __name__ == "__main__":
    try:
        nlp = spacy.load(MODEL)
    except OSError as e:
        raise SystemExit(
            f"Model {MODEL!r} not installed. For scispacy:\n"
            f"  pip install scispacy\n"
            f"  pip install https://s3-us-west-2.amazonaws.com/ai2-s2-scispacy/"
            f"releases/v0.5.4/{MODEL}-0.5.4.tar.gz"
        ) from e

    # Optional: expand biomedical abbreviations (e.g. CHC -> chronic hepatitis C).
    # Requires scispacy; safe to leave commented out.
    #   from scispacy.abbreviation import AbbreviationDetector
    #   nlp.add_pipe("abbreviation_detector")

    # ------------------------------------------------------------------
    # Input: a doccano JSONL export (text + gold [start, end, label] spans).
    # The human annotations act as the typed NER layer, aligned onto each
    # parsed doc by character offset via spans_from_labels().
    # Defaults to test_sentences.jsonl at the repo root; override with an arg:
    #   python3 code/text_mining_models/spacy_dependency.py path/to/export.jsonl
    # ------------------------------------------------------------------
    if len(sys.argv) > 1:
        data_path = sys.argv[1]
    elif here is not None:
        data_path = str(here("test_sentences.jsonl"))
    else:
        repo_root = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
        data_path = os.path.join(repo_root, "test_sentences.jsonl")
    if not os.path.exists(data_path):
        raise SystemExit(f"doccano file not found: {data_path}")

    # Carry (id, gold_spans) alongside each text through nlp.pipe via as_tuples.
    records = list(load_doccano(data_path))
    stream = ((text, (rid, spans)) for rid, text, spans in records)

    def fmt_ents(pairs):
        """[(text, label), ...] -> 'text (LABEL); text (LABEL)' for a CSV cell."""
        return "; ".join(f"{t} ({lab})" for t, lab in pairs)

    n_rel = 0
    saved = []      # only relations that overlap >=1 gold annotation
    n_pairs = []    # part-of relations between annotated counts
    for doc, (rid, spans) in nlp.pipe(stream, as_tuples=True):
        ents = spans_from_labels(doc, spans)   # doccano gold spans -> entities
        rows = relations_with_entities(doc, ents)
        if not rows and not os.environ.get("DEBUG"):
            continue

        snippet = doc.text[:90].replace("\n", " ")
        print(f"\n# [{rid}] {snippet}{'...' if len(doc.text) > 90 else ''}")
        if os.environ.get("DEBUG"):
            print(f"    gold entities: {[(e.text, e.label_) for e in ents]}")

        for row in rows:
            n_rel += 1
            print(f"  {row['subject']!r:30} --{row['relation']:16}--> {row['object']!r}")
            if row["subject_entities"]:
                print(f"{'':6}subj entities: {row['subject_entities']}")
            if row["object_entities"]:
                print(f"{'':6} obj entities: {row['object_entities']}")

            # Keep only relations where an argument overlaps a gold annotation.
            if row["subject_entities"] or row["object_entities"]:
                saved.append({
                    "id": rid,
                    "subject": row["subject"],
                    "subject_entities": fmt_ents(row["subject_entities"]),
                    "relation": row["relation"],
                    "object": row["object"],
                    "object_entities": fmt_ents(row["object_entities"]),
                })

        # Part-of relations between the annotated counts (N labels).
        for c in number_containment(doc, number_ents=ents):
            print(f"      N: {c['child_n']} ({c['child']}) "
                  f"part-of {c['parent_n']} ({c['parent']})  [{c['rule']}]")
            n_pairs.append({"id": rid, **c})

    # Write outputs to CSV (entity relations + numeric part-of relations).
    def _out(name):
        return (str(here(f"output/text_mining_predictions/{name}"))
                if here is not None
                else os.path.join(os.path.dirname(data_path), name))

    rel_path = _out("entity_relations.csv")
    os.makedirs(os.path.dirname(rel_path), exist_ok=True)
    with open(rel_path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["id", "subject", "subject_entities",
                                               "relation", "object", "object_entities"])
        writer.writeheader()
        writer.writerows(saved)

    # Group numeric part-of relations by (sentence, parent) -> list of children.
    grouped = {}
    order = []
    for p in n_pairs:
        key = (p["sentence"], p["parent"])
        if key not in grouped:
            grouped[key] = {"sentence": p["sentence"],
                            "N_parent": p["parent"],
                            "N_children": []}
            order.append(key)
        if p["child"] not in grouped[key]["N_children"]:
            grouped[key]["N_children"].append(p["child"])
    containment = [grouped[k] for k in order]

    n_path = _out("number_containment.json")
    with open(n_path, "w", encoding="utf-8") as f:
        json.dump(containment, f, indent=2, ensure_ascii=False)

    print(f"\n{len(records)} records, {n_rel} relations extracted.")
    print(f"  {len(saved)} annotation-overlapping relations -> {rel_path}")
    print(f"  {len(containment)} numeric parent groups        -> {n_path}")
