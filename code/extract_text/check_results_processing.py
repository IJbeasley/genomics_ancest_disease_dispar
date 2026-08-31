#!/usr/bin/env python3
"""
Validate preprocessing quality of extracted results text.
Quick checks to ensure text is ready for NLP/sentence tokenization.

Companion to check_methods_processing.py — same checks and same exit-code
contract (0 = clean or warnings only, 1 = issues found), plus a few checks
specific to results sections:

  * section-boundary leakage (methods / discussion prose bleeding in)
  * presence of quantitative content (p-values, ORs, CIs, sample sizes)
  * whether the text is only an abstract-length stub

Files whose name ends in '_main' were produced by the whole-body fallback in
extract_results.py (Nature-style "Main" articles, letters to the editor).
Those legitimately contain methods and discussion prose, so the leakage check
is reported as a note rather than an issue.
"""

import sys
import os
import re


# Opening phrases that suggest the extractor ran past the end of the results
# section, or started before its beginning.
_METHODS_LEAKAGE_PATTERNS = (
    r'\bwritten informed consent was obtained\b',
    r'\bthe study (?:was|protocol was) approved by\b',
    r'\bapproved by the (?:institutional review board|ethics committee)\b',
    r'\bgenomic dna was extracted\b',
    r'\bwere genotyped using the\b',
    r'\bimputation was (?:performed|carried out) using\b',
    r'\ball (?:statistical )?analyses were (?:performed|conducted) (?:using|with)\b',
)

_DISCUSSION_LEAKAGE_PATTERNS = (
    r'\b(?:to our knowledge|to the best of our knowledge), this is the (?:first|largest)\b',
    r'\bthis study has several (?:limitations|strengths)\b',
    r'\bour (?:study|findings) (?:has|have) (?:several )?limitations\b',
    r'\bfurther (?:studies|research|work) (?:are|is) (?:needed|warranted|required)\b',
    r'\bin conclusion\b',
    r'\bin summary, (?:we|our|these)\b',
)


def check_file(filepath):
    """Run all validation checks on a file."""

    with open(filepath, 'r', encoding='utf-8') as f:
        text = f.read()

    is_main_fallback = os.path.basename(filepath).replace('.txt', '').endswith('_main')

    print("=" * 60)
    print(f"PREPROCESSING VALIDATION: {filepath}")
    if is_main_fallback:
        print("(whole-body fallback — section-boundary checks relaxed)")
    print("=" * 60)
    print()

    # Track issues
    issues = []
    warnings = []
    notes = []

    # 1. Citation artifacts
    print("📚 CITATION CLEANUP:")
    empty_brackets = len(re.findall(r'\[\s*\]', text))
    empty_parens = len(re.findall(r'\(\s*\)', text))
    et_al_cites = len(re.findall(r'\([A-Z][a-zA-Z\s&,;.]+et al[,;\s.]*\)', text))
    number_brackets = len(re.findall(r'\[\s*\d+\s*(?:,\s*\d+\s*)*\]', text))

    print(f"   Empty brackets []: {empty_brackets}")
    print(f"   Empty parens (): {empty_parens}")
    print(f"   'et al' citations: {et_al_cites}")
    print(f"   Number brackets [1,2]: {number_brackets}")

    if empty_brackets > 0: issues.append(f"{empty_brackets} empty brackets")
    if empty_parens > 0: issues.append(f"{empty_parens} empty parentheses")
    if et_al_cites > 0: issues.append(f"{et_al_cites} et al citations")
    if number_brackets > 0: warnings.append(f"{number_brackets} numbered citations")
    print()

    # 2. Spacing issues
    print("📏 SPACING:")
    space_period = len(re.findall(r' \.', text))
    space_comma = len(re.findall(r' ,', text))
    space_semicolon = len(re.findall(r' ;', text))
    double_space = len(re.findall(r'  +', text))
    space_in_parens = len(re.findall(r'\(\s+|\s+\)', text))

    print(f"   Space before period ' .': {space_period}")
    print(f"   Space before comma ' ,': {space_comma}")
    print(f"   Space before semicolon ' ;': {space_semicolon}")
    print(f"   Multiple spaces: {double_space}")
    print(f"   Extra space in parens: {space_in_parens}")

    if space_period > 0: issues.append(f"{space_period} spaces before periods")
    if space_comma > 0: issues.append(f"{space_comma} spaces before commas")
    if space_semicolon > 0: issues.append(f"{space_semicolon} spaces before semicolons")
    if double_space > 50: warnings.append(f"{double_space} double spaces (unusual)")
    if space_in_parens > 0: issues.append(f"{space_in_parens} extra spaces in parentheses")
    print()

    # 3. Punctuation issues
    print("🔤 PUNCTUATION:")
    double_period = len(re.findall(r'\.\.+', text))
    double_comma = len(re.findall(r',,+', text))
    dash_comma = len(re.findall(r'[–—\-]\s*,', text))

    print(f"   Double periods '..': {double_period}")
    print(f"   Double commas ',,': {double_comma}")
    print(f"   Dash-comma '–,': {dash_comma}")

    if double_period > 0: issues.append(f"{double_period} double periods")
    if double_comma > 0: issues.append(f"{double_comma} double commas")
    if dash_comma > 0: issues.append(f"{dash_comma} dash-comma artifacts")
    print()

    # 4. LaTeX/markup noise
    print("🧹 LaTeX/MARKUP CLEANUP:")
    usepackage = len(re.findall(r'\\usepackage', text))
    documentclass = len(re.findall(r'\\documentclass', text))
    begin_doc = len(re.findall(r'\\begin\{document\}', text))

    print(f"   \\usepackage commands: {usepackage}")
    print(f"   \\documentclass: {documentclass}")
    print(f"   \\begin{{document}}: {begin_doc}")

    if usepackage > 0: issues.append(f"{usepackage} LaTeX usepackage commands")
    if documentclass > 0: issues.append(f"{documentclass} LaTeX documentclass")
    if begin_doc > 0: issues.append(f"{begin_doc} LaTeX begin commands")
    print()

    # 5. Section numbers
    print("🔢 SECTION NUMBERING:")
    section_nums = len(re.findall(r'(?:^|\. )(\d+\.)+\d*\s+[A-Z]', text))

    print(f"   Section numbers (e.g., '3.1 Results'): {section_nums}")

    if section_nums > 0: warnings.append(f"{section_nums} section numbers found")
    print()

    # 6. Section-boundary leakage (results-specific)
    print("🚧 SECTION BOUNDARIES:")
    methods_hits = [p for p in _METHODS_LEAKAGE_PATTERNS
                    if re.search(p, text, re.IGNORECASE)]
    discussion_hits = [p for p in _DISCUSSION_LEAKAGE_PATTERNS
                       if re.search(p, text, re.IGNORECASE)]

    print(f"   Methods-like passages: {len(methods_hits)}")
    print(f"   Discussion-like passages: {len(discussion_hits)}")

    if is_main_fallback:
        if methods_hits or discussion_hits:
            notes.append("whole-body text includes methods/discussion prose (expected)")
    else:
        if methods_hits:
            warnings.append(f"{len(methods_hits)} methods-like passage(s) — check start boundary")
        if discussion_hits:
            warnings.append(f"{len(discussion_hits)} discussion-like passage(s) — check end boundary")
    print()

    # 7. Quantitative content (results-specific)
    print("🔬 QUANTITATIVE CONTENT:")
    p_values = len(re.findall(r'\b[Pp]\s*(?:value)?\s*[<=>≤≥]\s*\d', text))
    effect_sizes = len(re.findall(r'\b(?:OR|HR|RR|beta|β)\s*[=:]\s*[-−]?\d', text))
    conf_ints = len(re.findall(r'\b\d+\s*%\s*CI\b', text, re.IGNORECASE))
    rsids = len(re.findall(r'\brs\d{3,}\b', text))
    tables_figs = len(re.findall(r'\b(?:Table|Figure|Fig\.?)\s*\d', text))

    print(f"   p-values: {p_values}")
    print(f"   Effect sizes (OR/HR/beta): {effect_sizes}")
    print(f"   Confidence intervals: {conf_ints}")
    print(f"   rsIDs: {rsids}")
    print(f"   Table/Figure references: {tables_figs}")

    quantitative = p_values + effect_sizes + conf_ints + rsids
    if quantitative == 0:
        warnings.append("no p-values, effect sizes, CIs or rsIDs — may not be a results section")
    print()

    # 8. Text statistics
    print("📊 STATISTICS:")
    word_count = len(text.split())
    sentence_count = len(re.findall(r'[.!?]+\s+', text))
    avg_sentence_len = word_count / max(sentence_count, 1)

    print(f"   Total words: {word_count:,}")
    print(f"   Estimated sentences: {sentence_count:,}")
    print(f"   Avg words/sentence: {avg_sentence_len:.1f}")

    if word_count < 100:
        issues.append(f"only {word_count} words — likely an abstract stub, not a results section")
    elif word_count < 250:
        warnings.append(f"only {word_count} words — unusually short results section")
    print()

    # 9. Sample sentences
    print("📝 SAMPLE (first 3 sentences):")
    sentences = re.split(r'[.!?]+\s+', text)[:3]
    for i, sent in enumerate(sentences, 1):
        preview = sent[:100] + "..." if len(sent) > 100 else sent
        print(f"   {i}. {preview}")
    print()

    # Final verdict
    print("=" * 60)
    print("🏁 VERDICT:")
    print("=" * 60)

    if notes:
        print("ℹ️  NOTES:")
        for note in notes:
            print(f"   • {note}")
        print()

    if issues:
        print("❌ ISSUES FOUND:")
        for issue in issues:
            print(f"   • {issue}")
        print("\n⚠️  Text needs additional cleaning!")
        return False
    elif warnings:
        print("⚠️  WARNINGS:")
        for warning in warnings:
            print(f"   • {warning}")
        print("\n✅ Text is acceptable but check warnings")
        return True
    else:
        print("✅ PASSED - Text is clean and ready for NLP!")
        return True


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: python3 check_results_processing.py <results_file.txt>")
        sys.exit(1)

    filepath = sys.argv[1]
    success = check_file(filepath)
    sys.exit(0 if success else 1)
