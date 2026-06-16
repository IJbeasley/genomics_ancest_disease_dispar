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
import os
import spacy

# Change this to "en_core_web_sm" to fall back to the general-English model.
MODEL = "en_core_sci_sm"

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


def head_np_span(token):
    """Noun phrase *without* trailing relative clauses / heavy modifiers.

    Keeps the immediate determiners/adjectives/compounds/numbers so we get
    '224 Japanese CHC patients' but not the attached 'who were treated ...'.
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
    return token.doc[start:end + 1].text


def extract_relations(doc):
    """Yield (subject, relation, object) triples for every verb in the doc.

    `relation` is the verb lemma, with the preposition appended for
    prepositional objects (e.g. 'obtain:from', 'treat:at').
    """
    triples = []
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
                    subjects.append(head_np_span(verb.head))
                else:
                    subjects.append(head_np_span(c))

            # Passive agent: "... obtained by <X>" -> subject is the agent.
            # ClearNLP: child dep == 'agent' (the 'by'), pobj underneath.
            # UD:       handled by the obl/case path below; skip here.
            for c in verb.children:
                if c.dep_ == "agent":
                    subjects += [head_np_span(p) for p in c.children if p.dep_ == "pobj"]

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
                subjects = [head_np_span(verb.head)]

            # Drop temporal/numeric objects ("From 2009 to 2012"), keeping cohorts.
            objects = [(rel, head_np_span(tok)) for rel, tok in objects
                       if not (DROP_NUMERIC_OBJECTS and tok.pos_ == "NUM")]

            for subj in (subjects or [None]):
                for rel, obj in objects:
                    triples.append((subj, rel, obj))
    return triples


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

    texts = (
        "From 2009 to 2012, samples for the GWAS were obtained from 224 Japanese "
        "CHC patients who were treated at 11 multi-center hospitals "
        "(liver units with hepatologists) throughout Japan.",
        "In the following stage of replication analysis, 160 samples were "
        "collected from an independent set of Japanese CHC patients.",
        "This study was completed in a Mexican-American population from Starr County, Texas.",
        "We selected as unrelated cases 291 individuals who represent the youngest age-at-onset individuals from the multiplex families in our previous linkage studies and for whom we have the richest phenotypic data.",
        "Rather, they are a representative sample of 323 unrelated individuals drawn from a random survey of Starr County.",
        "Of this case and random control set, 281 and 280 individuals were analyzed (see “Quality control” below) and are described in Table 1.",
        "The datasets from the GoDARTS project were analysed in this study. ",
        "The GoDARTS project mainly recruits type 2 diabetic patients and non‐diabetic controls throughout Tayside, Scotland, to identify genetic susceptibility to diabetes including its complications and response to treatment.",
        "So far, the project has recruited 9,439 diabetic patients and 6,927 of them have been genotyped. ",
        "For this study, we extracted the DR screening records of all GoDARTS individuals from June 1996 until June 2011 as well as information on age, gender, body mass index (BMI), HbA1c and duration of diabetes.",
        "Meta‐analyses were performed in four studies of Caucasian patients with type 2 diabetes (The Scania Diabetes Registry, The Australian DR Genetics Case‐Control Study, The Blue Mountain Eye Study and Cardiovascular Health Study 2) and two studies of Caucasian patients with type 1 diabetes (The Finnish Diabetic Nephropathy Study and The Genetics of Kidneys in Diabetes study/The Epidemiology of Diabetes Interventions and Complications).",
        "Participants for this study were recruited in the diabetes centres in Singapore as described previously in our Diabetic Nephropathy (DN) cohort.",
        "327 participants (250 Chinese and 77 Malays) from the DN study were subjected to genome wide association study.",
        "Briefly, samples with call-rate<95.0% (N=4), extremes in heterozygozity (>3 SD from mean, N=55) and known duplicates (N= 2) were excluded from analyses."
    )
    # nlp.pipe() processes an iterable of texts efficiently (batched).
    for text, doc in zip(texts, nlp.pipe(texts)):
        print(f"\n# {text}")
        if os.environ.get("DEBUG"):
            # Dump the parse so you can see which dependency scheme the model uses.
            for t in doc:
                print(f"    {t.text:14} pos={t.pos_:6} dep={t.dep_:12} head={t.head.text}")
        for subj, rel, obj in extract_relations(doc):
            print(f"{subj!r:35} --{rel:18}--> {obj!r}")
