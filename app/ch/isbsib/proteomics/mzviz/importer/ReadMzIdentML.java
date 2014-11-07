package ch.isbsib.proteomics.mzviz.importer;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.*;
import java.util.Map.Entry;

import org.expasy.mzjava.proteomics.io.ms.ident.MzIdentMlReader;
import org.expasy.mzjava.proteomics.io.ms.ident.PSMReaderCallback;
import org.expasy.mzjava.proteomics.ms.ident.PeptideMatch;
import org.expasy.mzjava.proteomics.ms.ident.SpectrumIdentifier;

public class ReadMzIdentML {
	
	static private Map<SpectrumIdentifier, List<PeptideMatch>> searchResultMap = new HashMap<SpectrumIdentifier, List<PeptideMatch>>();
		
    static PSMReaderCallback insertIdResultCB = new PSMReaderCallback() {
        public void resultRead(SpectrumIdentifier identifier, PeptideMatch searchResult) {

            List<PeptideMatch> results;
            if (searchResultMap.containsKey(identifier)) {
                results = searchResultMap.get(identifier);
            } else {
                results = new ArrayList<PeptideMatch>();
                searchResultMap.put(identifier, results);
            }

            results.add(searchResult);
        }
    };
	
	
	public static void main(String[] args) {
		
		try {
			InputStream fr = new FileInputStream(args[0]);	
			MzIdentMlReader reader = new MzIdentMlReader();
			
			reader.parse(fr, insertIdResultCB);
			searchResultMap.entrySet();
			
			for(Entry<SpectrumIdentifier, List<PeptideMatch>> res : searchResultMap.entrySet()){
				
				System.out.println("## Spec ######");
				SpectrumIdentifier id = res.getKey();
				System.out.println(id.getSpectrum());
				
				System.out.println("## Pep ######");
				PeptideMatch pep = res.getValue().get(0);
				System.out.println(pep.getScore("Mascot:score"));
				System.out.println(pep.toString());
				
				System.out.println("## Prot ######");
				System.out.println(pep.getProteinMatches().get(0).getAccession());
				
			}
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
	}

}
