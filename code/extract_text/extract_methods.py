#!/usr/bin/env python3
"""
Extract methods section from JATS XML files.
This script extracts all text content from sections marked with sec-type="methods".
"""

import xml.etree.ElementTree as ET
import argparse
import sys
from pathlib import Path


def extract_text_from_element(element, parent_tag=None):
    """
    Recursively extract all text content from an XML element,
    including nested elements, while preserving structure with line breaks.
    """
    text_parts = []
    current_tag = element.tag.split('}')[-1] if '}' in element.tag else element.tag
    
    # Check if this is a section with a title
    if current_tag == 'sec':
        # Look for label and title elements
        label_elem = None
        title_elem = None
        
        for child in element:
            child_tag = child.tag.split('}')[-1] if '}' in child.tag else child.tag
            if child_tag == 'label':
                label_elem = child
            elif child_tag == 'title':
                title_elem = child
        
        # Add section header with period at the end
        if label_elem is not None or title_elem is not None:
            header_parts = []
            # Skip label (section numbers) for cleaner sentence tokenization
            # if label_elem is not None:
            #     # Recursively get all text from label
            #     label_text = extract_text_from_element(label_elem, current_tag)
            #     if label_text:
            #         header_parts.append(label_text.strip())
            if title_elem is not None:
                # Recursively get all text from title (including nested elements)
                title_text = extract_text_from_element(title_elem, current_tag)
                if title_text:
                    # Remove leading section numbers from title (e.g., "2.1 GWAS" → "GWAS")
                    import re
                    title_text = re.sub(r'^(\d+\.)*\d+\s+', '', title_text.strip())
                    if title_text:  # Only add if there's text left after removing numbers
                        header_parts.append(title_text)
            
            if header_parts:
                text_parts.append(' '.join(header_parts) + '. ')
        
        # Process other children (except label and title which we already handled)
        for child in element:
            child_tag = child.tag.split('}')[-1] if '}' in child.tag else child.tag
            if child_tag not in ['label', 'title']:
                child_text = extract_text_from_element(child, current_tag)
                if child_text:
                    text_parts.append(child_text)
            
            # Get the tail text (text after the child element)
            if child.tail:
                tail_text = child.tail.strip()
                if tail_text:
                    text_parts.append(tail_text)
    
    # Check if this is a paragraph - add spacing between paragraphs
    elif current_tag == 'p':
        # Get all text from the paragraph
        para_text_parts = []
        
        if element.text:
            para_text_parts.append(element.text.strip())
        
        for child in element:
            # Skip citation references (xref with ref-type="bibr")
            child_tag = child.tag.split('}')[-1] if '}' in child.tag else child.tag
            if child_tag == 'xref' and child.get('ref-type') == 'bibr':
                # Skip this citation, but keep any tail text
                if child.tail:
                    tail_text = child.tail.strip()
                    if tail_text:
                        para_text_parts.append(tail_text)
                continue
            
            child_text = extract_text_from_element(child, current_tag)
            if child_text:
                para_text_parts.append(child_text)
            
            if child.tail:
                tail_text = child.tail.strip()
                if tail_text:
                    para_text_parts.append(tail_text)
        
        # Join paragraph parts and normalize internal whitespace
        import re
        import unicodedata
        
        para_text = ' '.join(filter(None, para_text_parts))
        
        # Normalize unicode
        para_text = unicodedata.normalize("NFKC", para_text)
        
        para_text = para_text.replace('\xa0', ' ')  # Replace non-breaking spaces with regular spaces
        para_text = re.sub(r'\s+', ' ', para_text).strip()
        
        # Remove section numbering at start of sentences (e.g., "2.1", "2.3.1", "2.5.6.7")
        para_text = re.sub(r'^(\d+\.)+\d*\s*', '', para_text)  # at start of paragraph
        para_text = re.sub(r'\.\s+(\d+\.)+\d*\s+', '. ', para_text)  # after period
        
        # Clean up punctuation artifacts from removed citations
        # Remove standalone punctuation like ", ," or ". ,"
        para_text = re.sub(r'([,;.])\s*([,;.])', r'\2', para_text)  # collapse repeated punctuation
        para_text = re.sub(r'\s+([,;.])', r'\1', para_text)  # remove space before punctuation
        
        # Remove incomplete author citations like (Author et al, ) or (Author et al,)
        # Also handles multiple citations: (Author et al,; Author2 et al, )
        para_text = re.sub(r'\([A-Z][a-zA-Z\s&,;.]+et al[,;\s.]*\)', '', para_text)
        
        # Remove empty brackets left by citations: [ ] or [, ] or [ , ] or [, – ]
        # This catches brackets with any combination of spaces, commas, semicolons, dashes
        para_text = re.sub(r'\[\s*[,;–—\-\s]*\s*\]', '', para_text)  # remove empty square brackets
        para_text = re.sub(r'\(\s*[,;–—\-\s]*\s*\)', '', para_text)  # remove empty round brackets
        para_text = re.sub(r'\s+', ' ', para_text).strip()  # normalize whitespace again
        
        # Clean up trailing punctuation artifacts like ",," or ", –," or "–," at end of sentences
        para_text = re.sub(r'[,;]\s*[–—\-]\s*[,;.]', '.', para_text)  # ", –." or "; –," → "."
        para_text = re.sub(r'[,;]\s*[–—\-]\s*$', '.', para_text)  # ", –" at end → "."
        para_text = re.sub(r'[–—\-]\s*[,;.]', '.', para_text)  # "–." or "–," → "."
        
        # Remove extra spaces inside brackets: "( text )" → "(text)"
        para_text = re.sub(r'\(\s+', '(', para_text)  # "( " → "("
        para_text = re.sub(r'\s+\)', ')', para_text)  # " )" → ")"
        para_text = re.sub(r'\[\s+', '[', para_text)  # "[ " → "["
        para_text = re.sub(r'\s+\]', ']', para_text)  # " ]" → "]"
        
        # Final cleanup: normalize any remaining whitespace and trim
        para_text = re.sub(r'\s+([,;.:!?])', r'\1', para_text)  # Remove space before punctuation
        para_text = re.sub(r'\s+', ' ', para_text).strip()
        
        # Replace double (or more) periods with single period
        para_text = re.sub(r'\.{2,}', '.', para_text)  # ".." or "..." → "."
        # Replace double (or more) commas with single comma
        para_text = re.sub(r',{2,}', ',', para_text)  # ",," or ",,," → ","
        
        
        # Add paragraph with space after it
        if para_text:
            text_parts.append(para_text + ' ')
    
    else:
        # For non-section, non-paragraph elements, process normally
        # Skip citation references entirely
        if element.tag.endswith('xref') and element.get('ref-type') == 'bibr':
            return ''  # Return empty string for citations
        
        # Skip LaTeX source in tex-math tags (but keep MathML)
        if element.tag.endswith('tex-math'):
            return ''  # Return empty string for LaTeX source
        
        # Skip graphics/images (we already have text from MathML)
        tag = element.tag.split('}')[-1] if '}' in element.tag else element.tag
        if tag in ['graphic', 'inline-graphic']:
            return ''  # Return empty string for images
        
        # Get the element's own text
        if element.text:
            text_parts.append(element.text.strip())
        
        # Process all child elements recursively
        for child in element:
            child_text = extract_text_from_element(child, current_tag)
            if child_text:
                text_parts.append(child_text)
            
            # Get the tail text (text after the child element)
            if child.tail:
                tail_text = child.tail.strip()
                if tail_text:
                    text_parts.append(tail_text)
    
    # Join all parts with spaces
    #result = ' '.join(filter(None, text_parts))
    result = ' '.join(text_parts)
    
    # Final global normalization
    import re
    import unicodedata
    
    result = unicodedata.normalize("NFKC", result)
    result = result.replace('\xa0', ' ')
    result = re.sub(r'\s+', ' ', result).strip()
    
    # Fix bracket spacing LAST
    result = re.sub(r'\(\s+', '(', result)
    result = re.sub(r'\s+\)', ')', result)
    
    # Replace double (or more) periods with single period
    result = re.sub(r'\.{2,}', '.', result)  # ".." or "..." → "."
    # Replace double (or more) commas with single comma
    result = re.sub(r',{2,}', ',', result)  # ",," or ",,," → ","
    
    # Remove empty brackets left by citations: [ ] or (, ) or [ , ] or ( , )
    result = re.sub(r'\[\s*[,;–—\-\s]*\s*\]', '', result)  # remove empty square brackets
    result = re.sub(r'\(\s*[,;–—\-\s]*\s*\)', '', result)  # remove empty round brackets
    result = re.sub(r'\s+', ' ', result).strip()  # normalize whitespace again
    
    # remove spaces before punctuation
    result = re.sub(r'\s+([,;.:!?])', r'\1', result)  # remove space before punctuation

    return result


def find_methods_section(root):
    """
    Find the methods section in a JATS XML document.
    Returns the methods section element or None if not found.
    
    Searches for methods section in multiple ways:
    1. sec-type="materials|methods" or "materials and methods" attribute
    2. sec-type="methods" attribute (not in abstract, prefer top-level)
    3. title text containing "methods" (case-insensitive)
    
    Skips methods sections inside abstracts (these are summaries, not full methods).
    """
    all_sections = find_all_methods_sections(root)
    return all_sections[0] if len(all_sections) > 0 else None


def find_all_methods_sections(root):
    """
    Find ALL top-level methods sections in a JATS XML document.
    Returns a list of methods section elements (may be empty).
    
    This is useful for files that have both a stub and a full methods section.
    Only returns top-level sections, not their subsections.
    Filters out author/contributor lists that are incorrectly tagged as methods.
    """
    candidates = []
    
    # Author/team section keywords to skip (these are not real methods)
    author_keywords = [
        'analysis team', 'author', 'contributor', 'writing group',
        'study group', 'consortium', 'working group', 'steering committee',
        'acknowledgment', 'funding', 'competing interest', 'conflict of interest'
    ]
    
    # Helper function to check if a section is inside another section in candidates
    def is_subsection_of_found(sec):
        """Check if sec is a descendant of any section already in candidates"""
        for found_sec in candidates:
            for descendant in found_sec.iter():
                if descendant == sec:
                    return True
        return False
    
    # Helper function to check if section is an author/contributor list
    def is_author_section(sec):
        """Check if this section is an author or contributor list"""
        for child in sec:
            if child.tag.endswith('title'):
                title_text = ''.join(child.itertext()).strip().lower()
                # Check if title matches author/team keywords
                for keyword in author_keywords:
                    if keyword in title_text:
                        return True
                break
        return False
    
    # First try: Look for sections with sec-type="materials|methods" or "materials and methods"
    for sec in root.iter():
        if sec.tag.endswith('sec'):
            sec_type = sec.get('sec-type')
            if sec_type and ('material' in sec_type.lower() and 'method' in sec_type.lower()):
                # Check if in abstract
                for parent in root.iter():
                    if parent.tag.endswith('abstract'):
                        for child in parent.iter():
                            if child == sec:
                                break
                        else:
                            continue
                        break
                else:
                    # Not in abstract, check if it's an author section
                    if not is_subsection_of_found(sec) and not is_author_section(sec):
                        candidates.append(sec)
    
    # Second try: Look for sections with sec-type="methods" (not in abstract)
    methods_candidates = []
    for sec in root.iter():
        if sec.tag.endswith('sec'):
            sec_type = sec.get('sec-type')
            if sec_type and sec_type.lower() == 'methods':
                # Check if in abstract
                is_in_abstract = False
                for parent in root.iter():
                    if parent.tag.endswith('abstract'):
                        for child in parent.iter():
                            if child == sec:
                                is_in_abstract = True
                                break
                        if is_in_abstract:
                            break
                
                if not is_in_abstract:
                    # Count depth (how many sec ancestors)
                    depth = 0
                    for parent in root.iter():
                        if parent.tag.endswith('sec'):
                            for child in parent.iter():
                                if child == sec and child != parent:
                                    depth += 1
                                    break
                    methods_candidates.append((sec, depth))
    
    # Sort by depth and add to candidates
    if methods_candidates:
        methods_candidates.sort(key=lambda x: x[1])
        for sec, depth in methods_candidates:
            if sec not in candidates and not is_subsection_of_found(sec) and not is_author_section(sec):
                candidates.append(sec)
    
    # Third try: Look for sections with title containing "methods" (not in abstract)
    # Common patterns: "Methods", "Materials and Methods", "Methods and Materials"
    for sec in root.iter():
        if sec.tag.endswith('sec'):
            # Check if inside abstract first
            is_in_abstract = False
            for parent in root.iter():
                if parent.tag.endswith('abstract'):
                    for child in parent.iter():
                        if child == sec:
                            is_in_abstract = True
                            break
                    if is_in_abstract:
                        break
            
            if is_in_abstract or sec in candidates or is_subsection_of_found(sec):
                continue
            
            # Look for a title child element
            for child in sec:
                if child.tag.endswith('title'):
                    title_text = ''.join(child.itertext()).strip().lower()
                    # Check if title contains methods-related keywords
                    if ('method' in title_text and 
                        not title_text.startswith('result') and
                        not title_text.startswith('discussion') and
                        # Avoid subsections like "Statistical methods"
                        len(title_text.split()) <= 6):
                        if not is_author_section(sec):
                            candidates.append(sec)
                    break  # Only check first title
    
    # Prioritize sections: prefer explicit "Methods Summary" / "Materials and Methods" titles
    # over generic sec-type="methods" sections that might be author lists
    def section_priority(sec):
        """Return priority score (lower is better)"""
        sec_type = sec.get('sec-type')
        for child in sec:
            if child.tag.endswith('title'):
                title = ''.join(child.itertext()).strip().lower()
                # Priority 1: Clear methods titles
                if ('methods summary' in title or 
                    'materials and methods' in title or
                    'methods and materials' in title):
                    return 1
                # Priority 2: Has sec-type with material+method
                if sec_type and 'material' in sec_type.lower() and 'method' in sec_type.lower():
                    return 2
                # Priority 3: Has sec-type="methods"
                if sec_type and sec_type.lower() == 'methods':
                    return 3
                # Priority 4: Title contains "method"
                if 'method' in title:
                    return 4
                break
        return 5
    
    # Sort by priority
    candidates.sort(key=section_priority)
    
    return candidates


def extract_methods_section(xml_file):
    """
    Extract the methods section text from a JATS XML file.
    
    Args:
        xml_file: Path to the JATS XML file
        
    Returns:
        String containing the methods section text, or None if not found or if online-only
    """
    try:
        tree = ET.parse(xml_file)
        root = tree.getroot()
        
        # Find the methods section
        methods_section = find_methods_section(root)
        
        if methods_section is None:
            # Check if methods are in supplementary materials
            supplementary_note = check_supplementary_methods(root)
            if supplementary_note:
                # Methods are in supplementary files - don't write a file
                print("Methods are in supplementary materials (not extracted).", file=sys.stderr)
                return None
            return None
        
        # Extract all text from the methods section
        methods_text = extract_text_from_element(methods_section)
        
        # Final cleanup: strip trailing whitespace
        methods_text = methods_text.strip()
        
        # Check if this is an online-only methods note (Nature journals, etc.)
        # Only flag if short AND clearly states methods are elsewhere
        if methods_text and len(methods_text.split()) < 50:
            lower_text = methods_text.lower()
            
            # Strong indicators that methods are NOT in this document
            # (not just that supplementary info exists)
            strong_indicators = [
                'available in the online version',
                'available at http',
                'available at https',
                'available at 10.',  # DOI references like 10.1038/...
                'available at doi',
                'online content',
            ]
            
            # Additional check: if it starts with phrases indicating redirection
            redirection_starts = [
                'methods and any',
                'any methods',
                'methods are available',
                'methods, including',
            ]
            
            has_strong_indicator = any(indicator in lower_text for indicator in strong_indicators)
            starts_with_redirection = any(lower_text.startswith(phrase) for phrase in redirection_starts)
            
            if has_strong_indicator or starts_with_redirection:
                # This looks like an online-only stub, but check if there's another methods section
                # (Some Nature papers have a stub followed by "ONLINE METHODS")
                all_methods_sections = find_all_methods_sections(root)
                if len(all_methods_sections) > 1:
                    # Try the next methods section
                    for section in all_methods_sections[1:]:
                        alt_text = extract_text_from_element(section).strip()
                        if alt_text and len(alt_text.split()) >= 50:
                            # Found a real methods section
                            return alt_text
                
                # No alternative found, this is truly online-only
                print("Methods are only available online (not extracted).", file=sys.stderr)
                return None
        
        return methods_text
        
    except ET.ParseError as e:
        print(f"Error parsing XML file: {e}", file=sys.stderr)
        return None
    except Exception as e:
        print(f"Error processing file: {e}", file=sys.stderr)
        return None


def check_supplementary_methods(root):
    """
    Check if methods section is in supplementary materials (common in Cell Press journals).
    Returns a note if supplementary methods are found, None otherwise.
    """
    # Look for supplementary-material sections
    for elem in root.iter():
        tag = elem.tag.split('}')[-1] if '}' in elem.tag else elem.tag
        if tag == 'supplementary-material' or (tag == 'sec' and elem.get('sec-type') == 'supplementary-material'):
            # Check all text in this element for methods references
            all_text = ' '.join(elem.itertext()).lower()
            if 'method' in all_text and ('supplemental' in all_text or 'supplementary' in all_text):
                # Found reference to supplementary methods
                # Try to extract the file name if available
                media_elem = elem.find('.//{*}media')
                if media_elem is not None:
                    href = media_elem.get('{http://www.w3.org/1999/xlink}href')
                    if href:
                        return f"NOTE: Methods section is in supplementary materials file: {href}\n\nThis XML file does not contain the methods text inline. Please refer to the supplementary materials document."
                
                return "NOTE: Methods section is in supplementary materials.\n\nThis XML file does not contain the methods text inline. Please refer to the supplementary materials document."
    
    return None


def main():
    parser = argparse.ArgumentParser(
        description='Extract methods section from JATS XML files'
    )
    parser.add_argument(
        'input_file',
        help='Path to the input JATS XML file'
    )
    parser.add_argument(
        '-o', '--output',
        help='Path to the output text file (if not specified, prints to stdout)'
    )
    
    args = parser.parse_args()
    
    # Extract methods section
    methods_text = extract_methods_section(args.input_file)
    
    if methods_text is None:
        print("No methods section found in the XML file.", file=sys.stderr)
        sys.exit(1)
    
    # Output the results
    if args.output:
        output_path = Path(args.output)
        output_path.write_text(methods_text, encoding='utf-8')
        print(f"Methods section extracted to: {args.output}")
    else:
        print(methods_text)


if __name__ == '__main__':
    main()
