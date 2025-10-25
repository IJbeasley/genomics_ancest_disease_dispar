import requests
import xml.etree.ElementTree as ET
from typing import List, Dict, Set

def get_dbgap_study_info(dbgap_id: str) -> Dict:
    """
    Fetch study information from dbGaP API.
    """
    url = f"https://www.ncbi.nlm.nih.gov/gap/rest/v1/studies/{dbgap_id}"
    
    try:
        response = requests.get(url, timeout=10)
        response.raise_for_status()
        data = response.json()
        return data
    except Exception as e:
        print(f"Error fetching dbGaP study {dbgap_id}: {e}")
    
    return {}


def extract_pubmed_ids_from_dbgap_json(study_data: Dict) -> Set[str]:
    """
    Extract PubMed IDs from dbGaP study JSON response.
    """
    pubmed_ids = set()
    
    # dbGaP API structure may vary; try common paths
    try:
        # Check study details
        if "studies" in study_data:
            for study in study_data["studies"]:
                # Look for publications field
                if "publications" in study:
                    for pub in study["publications"]:
                        if "pmid" in pub and pub["pmid"]:
                            pubmed_ids.add(str(pub["pmid"]))
                
                # Sometimes in study info
                if "study_info" in study:
                    info = study["study_info"]
                    if "publications" in info:
                        for pub in info["publications"]:
                            if "pmid" in pub and pub["pmid"]:
                                pubmed_ids.add(str(pub["pmid"]))
        
        # Direct publications field
        if "publications" in study_data:
            for pub in study_data["publications"]:
                if "pmid" in pub and pub["pmid"]:
                    pubmed_ids.add(str(pub["pmid"]))
    
    except Exception as e:
        print(f"Error parsing study data: {e}")
    
    return pubmed_ids


def get_pubmed_ids_from_dbgap_web(dbgap_id: str) -> Set[str]:
    """
    Fetch PubMed IDs from dbGaP web interface (more reliable for publications).
    Scrapes the study page for linked publications.
    """
    pubmed_ids = set()
    url = f"https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id={dbgap_id}"
    
    try:
        response = requests.get(url, timeout=10)
        response.raise_for_status()
        text = response.text
        
        # Look for PubMed links in the HTML
        # Pattern: PMID or pubmed links
        import re
        
        # Pattern 1: Direct PMID links
        pmid_pattern = r'pubmed["\']?\s*:\s*["\']?(\d+)'
        matches = re.findall(pmid_pattern, text, re.IGNORECASE)
        pubmed_ids.update(matches)
        
        # Pattern 2: PubMed URLs
        url_pattern = r'pubmed\.ncbi\.nlm\.nih\.gov[^"\']*[?&]id=(\d+)'
        matches = re.findall(url_pattern, text)
        pubmed_ids.update(matches)
        
        # Pattern 3: PMID= format
        pmid_eq_pattern = r'PMID[=:\s]+(\d+)'
        matches = re.findall(pmid_eq_pattern, text)
        pubmed_ids.update(matches)
    
    except Exception as e:
        print(f"Error fetching dbGaP web page for {dbgap_id}: {e}")
    
    return pubmed_ids


def get_pubmed_ids_from_pubmed_search(dbgap_id: str) -> Set[str]:
    """
    Search PubMed for papers that mention this dbGaP study ID.
    """
    pubmed_ids = set()
    
    # Search PubMed for dbGaP accession
    url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
    params = {
        "db": "pubmed",
        "term": dbgap_id,
        "retmax": 100,
        "rettype": "json",
        "retmode": "json"
    }
    
    try:
        response = requests.get(url, params=params, timeout=10)
        response.raise_for_status()
        data = response.json()
        
        if "esearchresult" in data:
            result = data["esearchresult"]
            if "idlist" in result:
                pubmed_ids.update(result["idlist"])
    
    except Exception as e:
        print(f"Error searching PubMed for {dbgap_id}: {e}")
    
    return pubmed_ids


def get_pubmed_ids_from_dbgap(dbgap_id: str, search_method: str = "all") -> Dict:
    """
    Main function to extract PubMed IDs associated with a dbGaP study.
    
    Args:
        dbgap_id: dbGaP study ID (e.g., "phs000001")
        search_method: "api" (dbGaP API), "web" (web scraping), 
                      "pubmed" (PubMed search), or "all" (try all methods)
    """
    all_pubmed_ids = set()
    sources = []
    
    if search_method in ["api", "all"]:
        print(f"Querying dbGaP API for {dbgap_id}...")
        study_data = get_dbgap_study_info(dbgap_id)
        api_ids = extract_pubmed_ids_from_dbgap_json(study_data)
        if api_ids:
            all_pubmed_ids.update(api_ids)
            sources.append("dbGaP API")
    
    if search_method in ["web", "all"]:
        print(f"Fetching dbGaP web page for {dbgap_id}...")
        web_ids = get_pubmed_ids_from_dbgap_web(dbgap_id)
        if web_ids:
            all_pubmed_ids.update(web_ids)
            sources.append("dbGaP web")
    
    if search_method in ["pubmed", "all"]:
        print(f"Searching PubMed for {dbgap_id}...")
        search_ids = get_pubmed_ids_from_pubmed_search(dbgap_id)
        if search_ids:
            all_pubmed_ids.update(search_ids)
            sources.append("PubMed search")
    
    return {
        "dbgap_id": dbgap_id,
        "pubmed_ids": sorted(list(all_pubmed_ids)),
        "sources": sources,
        "count": len(all_pubmed_ids)
    }


def batch_get_pubmed_ids_from_dbgap(dbgap_ids: List[str], search_method: str = "all") -> List[Dict]:
    """
    Extract PubMed IDs from multiple dbGaP studies.
    """
    results = []
    for i, dbgap_id in enumerate(dbgap_ids, 1):
        print(f"\n[{i}/{len(dbgap_ids)}] Processing {dbgap_id}...")
        result = get_pubmed_ids_from_dbgap(dbgap_id, search_method=search_method)
        results.append(result)
        print(f"  Found {result['count']} PubMed ID(s): {', '.join(result['pubmed_ids'])}")
    
    return results


# Example usage
if __name__ == "__main__":
    # Example dbGaP IDs
    dbgap_ids = ["phs001591.v1.p1"]
    
    results = batch_get_pubmed_ids_from_dbgap(dbgap_ids, search_method="all")
    
    # Print results
    for result in results:
        print(f"\ndbGaP ID: {result['dbgap_id']}")
        print(f"PubMed IDs: {result['pubmed_ids']}")
        print(f"Sources: {', '.join(result['sources'])}")
        print(f"Total: {result['count']}")