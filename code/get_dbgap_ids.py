import os
import re
import requests
from typing import List, Dict, Set
import xml.etree.ElementTree as ET


def get_paper_text_from_pubmed(pmid: str) -> Dict[str, str]:
    """
    Fetch paper title and abstract from PubMed using EFetch (XML).
    """
    url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
    params = {
        "db": "pubmed",
        "id": pmid,
        "rettype": "abstract",
        "retmode": "xml"
    }

    try:
        response = requests.get(url, params=params, timeout=10)
        response.raise_for_status()
        root = ET.fromstring(response.text)

        title = ""
        abstract = ""

        # Extract title
        article_title = root.find(".//ArticleTitle")
        if article_title is not None:
            title = article_title.text or ""

        # Extract abstract
        abstract_texts = root.findall(".//AbstractText")
        abstract = " ".join(a.text or "" for a in abstract_texts)

        return {"title": title.strip(), "abstract": abstract.strip()}

    except Exception as e:
        print(f"Error fetching PMID {pmid}: {e}")
        return {"title": "", "abstract": ""}


def get_full_text_from_europe_pmc(pmid: str, save_dir: str = "full_texts") -> str:
    """
    Fetch full text from Europe PMC (XML if available, else HTML fallback).
    """
    url = "https://www.ebi.ac.uk/europepmc/webservices/rest/search"
    params = {"query": f"PMID:{pmid}", "format": "json", "pageSize": 5}

    try:
        response = requests.get(url, params=params, timeout=10)
        response.raise_for_status()
        data = response.json()

        for article in data.get("resultList", {}).get("result", []):
            if str(article.get("pmid")) != str(pmid):
                continue  # skip unrelated records

            # Try XML full text first
            full_text_url = article.get("fullTextXmlUrl")
            if full_text_url:
                full_text = fetch_full_text_xml(full_text_url)
            else:
                # Fallback: HTML full text
                full_text_url = article.get("htmlUrl")
                if full_text_url:
                    print(f"Falling back to HTML full text for PMID {pmid}")
                    full_text = requests.get(full_text_url).text
                    full_text = re.sub(r"<[^>]+>", " ", full_text)  # strip HTML tags
                    full_text = re.sub(r"\s+", " ", full_text).strip()
                else:
                    continue  # no full text available

            # Save to file
            os.makedirs(save_dir, exist_ok=True)
            save_path = os.path.join(save_dir, f"{pmid}_fulltext.txt")
            with open(save_path, "w", encoding="utf-8") as f:
                f.write(full_text)
            print(f"Saved full text for PMID {pmid} → {save_path}")

            return full_text

        print(f"No full text found for PMID {pmid}")

    except Exception as e:
        print(f"Error fetching full text for PMID {pmid}: {e}")

    return ""



def fetch_full_text_xml(url: str) -> str:
    """
    Fetch and extract text from full text XML.
    """
    try:
        response = requests.get(url, timeout=10)
        response.raise_for_status()
        text = response.text
        # Strip XML tags and extra whitespace
        text = re.sub(r"<[^>]+>", " ", text)
        text = re.sub(r"\s+", " ", text)
        return text.strip()
    except Exception as e:
        print(f"Error fetching full text XML: {e}")
        return ""


def extract_dbgap_ids(text: str) -> Set[str]:
    """
    Extract dbGaP accession numbers (e.g., phs000001).
    """
    pattern = r"\bphs\d{6}\b"
    matches = re.findall(pattern, text, re.IGNORECASE)
    return {m.lower() for m in matches}


def get_dbgap_ids_from_pmid(pmid: str, use_full_text: bool = True, save_dir: str = "full_texts") -> Dict:
    dbgap_ids = set()
    sources = []

    os.makedirs(save_dir, exist_ok=True)
    save_path = os.path.join(save_dir, f"{pmid}_text.txt")

    print(f"Fetching metadata for PMID {pmid}...")
    metadata = get_paper_text_from_pubmed(pmid)
    combined_text = f"{metadata.get('title', '')}\n\n{metadata.get('abstract', '')}"
    
    # Save whatever text we have so far
    with open(save_path, "w", encoding="utf-8") as f:
        f.write(combined_text)
    print(f"Saved metadata text for PMID {pmid} → {save_path}")

    found_ids = extract_dbgap_ids(combined_text.lower())
    if found_ids:
        dbgap_ids.update(found_ids)
        sources.append("PubMed metadata")

    if use_full_text:
        print(f"Attempting to fetch full text for PMID {pmid}...")
        full_text = get_full_text_from_europe_pmc(pmid, save_dir=save_dir)
        if full_text:
            with open(save_path, "a", encoding="utf-8") as f:
                f.write("\n\n--- Europe PMC full text ---\n\n")
                f.write(full_text)
            found_ids = extract_dbgap_ids(full_text.lower())
            new_ids = found_ids - dbgap_ids
            if new_ids:
                dbgap_ids.update(new_ids)
                sources.append("Full text (Europe PMC)")

    return {
        "pmid": pmid,
        "dbgap_ids": sorted(list(dbgap_ids)),
        "sources": sources,
        "count": len(dbgap_ids),
    }


def batch_extract_dbgap_ids(pmids: List[str], use_full_text: bool = True, save_dir: str = "full_texts") -> List[Dict]:
    """
    Extract dbGaP IDs from multiple PubMed papers and save full texts.
    """
    results = []
    for i, pmid in enumerate(pmids, 1):
        print(f"\n[{i}/{len(pmids)}] Processing PMID {pmid}...")
        result = get_dbgap_ids_from_pmid(pmid, use_full_text=use_full_text, save_dir=save_dir)
        results.append(result)
        print(f"  Found {result['count']} dbGaP ID(s): {', '.join(result['dbgap_ids']) or 'none'}")
    return results


# Example usage
if __name__ == "__main__":
    pmids = ["29357356", "27226192", "5794186"]
    results = batch_extract_dbgap_ids(pmids, use_full_text=True)

    for result in results:
        print(f"\nPMID: {result['pmid']}")
        print(f"dbGaP IDs: {result['dbgap_ids']}")
        print(f"Sources: {', '.join(result['sources']) or 'none'}")

