package theAssistantDesRaffinages;

import java.lang.String;
import java.util.LinkedList;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.google.gson.JsonArray;


import javax.swing.JTree;
import javax.swing.tree.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.nio.charset.StandardCharsets;

public class Export {
	
	public Export() {};
	
	//passage au txt manuel
	public String getString(JTree tree) {
		String string;
		LinkedList<Integer> list;
		HashMap<Integer, Element> references;
		
        RaffinageMutableTreeNode selectedNode = (RaffinageMutableTreeNode) tree.getModel().getRoot();
        ActionComplexe actionComplexe = selectedNode.getActionComplexe();
        
        // On peut utiliser les méthodes et attributs de la classe ActionComplexe
        
        string = actionComplexe.getTitreEntier();
        list = (LinkedList<Integer>)actionComplexe.getElements();
        string = before() + "\\textbf{R0 : }" + actionComplexe.getTitre().replaceAll("<(\\d+\\w+)>|</(\\d+\\w+)>", "") + "\\\\ \r\n \\>\\hspace{2em}\\\\ \r\n" + miseEnPage(parse_recursif(actionComplexe)) +" \n \\end{tabbing} \n \\end{document}";
        //System.out.println("String written: " + string);
		return string;
	}
	
	public boolean allElementary(LinkedList<Element> list) {
		for ( Element e : list) {
			if (e instanceof ActionComplexe) {
        		return false;
        	}
        }
		return true;
	}
	
	public String parse_recursif(ActionComplexe ac) {
		String body = "";
		String title;
	    MessageDigest md;
	    int hash = 0;
		try {
			md = MessageDigest.getInstance("MD5");
		    byte[] hashBytes = md.digest(ac.getTitre().getBytes(StandardCharsets.UTF_8));
		    hash = bytesToInt(hashBytes);
		} catch (NoSuchAlgorithmException e) {
			e.printStackTrace();
		}
		title = "{ \\label{" + String.valueOf(hash) + "}{ \\textbf{R" + String.valueOf(ac.getNiveau())+ " : Comment \"}" + ac.getTitre().replaceAll("<(?:s|r|rr|t|rg)>", "") + "\\textbf{\" ?}}}\n";
		List<ActionComplexe> liste = new ArrayList<>();
        HashMap<Integer, Element> ref = (HashMap<Integer, Element>)ac.getReferences();
		
        for ( Integer a : ac.getElements()) {
        	Element e = ref.get(a);
        	if (!(e instanceof StructureDeControle)) {
        		if (e instanceof ActionComplexe) {
        			System.out.println(e.toString());
	        		String current  = ((ActionComplexe) e).getTitreEntier().replaceAll("<(\\d+\\w+)>|</(\\d+\\w+)>", "").replaceAll("\n", "");
	        		try {
	        			md = MessageDigest.getInstance("MD5");
	        		    byte[] hashBytes = md.digest(((ActionComplexe) e).getTitre().getBytes(StandardCharsets.UTF_8));
	        		    hash = bytesToInt(hashBytes);
	        		} catch (NoSuchAlgorithmException e1) {
	        			e1.printStackTrace();
	        		}
	        		body += " {\\hyperlink{"+ String.valueOf(hash) + "}{" + current.substring(13,current.length()-1) + "}}\n";
	        		liste.add((ActionComplexe) e);
        		} else {
            		body += e.toString().replaceAll("<(\\d+\\w+)>|</(\\d+\\w+)>", "") + "\n";
        		}
        	} else {
        		body += e.toString().replaceAll("<(\\d+\\w+)>|</(\\d+\\w+)>", "").replace("FinTQ","\nFinTQ") + "\n";
        	}
        }	
        if (liste.size()>0) {
	        for (ActionComplexe e : liste) {
	        	body += parse_recursif((ActionComplexe) e)+"\n";
	        }
        }
        return title + body;
	}
	
	
	public String miseEnPage(String chaine) {
		String[] lines = chaine.split("\n");
		for (int i = 0; i < lines.length; i++) {
		    if (!lines[i].startsWith("{ \\label")) {
		        lines[i] = "\\>\\hspace{2em}" + lines[i];
		    }
		    else {
		    	if (i>0) {
		    		lines[i-1] = lines[i-1].replace("\\\\", "\\\\[3ex]");
		    	}
		    	else if (lines[i].contains("Fin")) {
		    		int j=i;
		    		boolean isEndControle = false;
		    		while (!isEndControle) {
		    			j -- ;
		    			isEndControle = (lines[j].contains("Si") || lines[j].contains("Tant") || lines[j].contains("Pour") || lines[j].contains("Faire"));
		    			if (!isEndControle) {
		    				lines[j]=lines[j].replace("\\hspace{2em}", "\\\\hspace{2em}\\hspace{2em}");
		    			}
		    			
		    		}
		    	}
		    }
		    lines[i] = lines[i] + "\\\\" ;
		}
		
		//Remove all empty lines
		List<String> resultList = new ArrayList<>();
		for (String line : lines) {
		    if (line.length()>5){
		        resultList.add(line);
		    }
		}
		return String.join("\n", resultList);
	}
	
	// Helper method to convert bytes to int
	public static int bytesToInt(byte[] bytes) {
	    int result = 0;
	    for (byte b : bytes) {
	        result = (result << 8) | (b & 0xFF);
	    }
	    return result;
	}
	
/*
	//GSON
	public String getString2(JTree tree) {
		DefaultTreeModel treeModel = (DefaultTreeModel) tree.getModel();
		TreeNode rootNode = (TreeNode) treeModel.getRoot();

		// Convert the tree structure to a serializable format
		JsonObject treeJson = convertTreeToJson(rootNode);

		// Serialize the JSON object using Gson
		Gson gson = new GsonBuilder().setPrettyPrinting().create();
		String jsonString = gson.toJson(treeJson);

		System.out.println(jsonString);
		return jsonString;
	}
	
	// Method to recursively convert the tree to a JSON object
	final JsonObject convertTreeToJson(TreeNode node) {
	    JsonObject nodeJson = new JsonObject();
	    int count = 0;
	    while (node.children().hasMoreElements()) {
	    	node.children().nextElement();
	        count++;
	    }
	    System.out.println("Length of enumeration: " + count);
	    
	    nodeJson.addProperty("name", node.toString());
	    JsonArray childrenJson = new JsonArray();
	    int childCount = node.getChildCount();
	    System.out.println("Nombres d'enfants : " + String.valueOf(childCount));
	    for (int i = 0; i < childCount; i++) {
	        TreeNode childNode = node.getChildAt(i);
	        JsonObject childJson = convertTreeToJson(childNode);
	        childrenJson.add(childJson);
	    }
	    nodeJson.add("children", childrenJson);
	    return nodeJson;
	}
*/
	

    public String before() {
    	return "\\documentclass[11pt,french]{article}\r\n"
    			+ "\\usepackage[utf8]{inputenc}\r\n"
    			+ "\\usepackage[T1]{fontenc}\r\n"
    			+ "\\usepackage{amsmath}\r\n"
    			+ "\\usepackage{amsfonts}\r\n"
    			+ "\\usepackage{babel}\r\n"
    			+ "\\usepackage{amssymb}\r\n"
    			+ "\\usepackage{array}\r\n"
    			+ "\\usepackage{fancyhdr}\r\n"
    			+ "\\usepackage{fullpage}\r\n"
    			+ "\\usepackage{geometry}\r\n"
    			+ "\\usepackage{xcolor}\r\n"
    			+ "\\usepackage{graphicx}\r\n"
    			+ "\\usepackage{hyperref}\r\n"
    			+ "\\usepackage{esint}\r\n"
    			+ "\\usepackage{bbold}\r\n"
    			+ "\\usepackage{mathtools}\r\n"
    			+ "\\usepackage{parskip}\r\n"
    			+ "\r\n"
    			+ "\\geometry{a4paper, left=5mm, right=5mm, top=2mm, bottom=20mm}\r\n"
    			+ "\\pagestyle{fancy} \r\n"
    			+ "\r\n"
    			+ "\r\n"
    			+ "\\lhead{\\href{https://github.com/newtondotcom/RaffIDE}{RaffIDE}} \r\n"
    			+ "\\chead{Raffinages} \r\n"
    			+ "\\rhead{\\thepage} \r\n"
    			+ "\\lfoot{\\href{https://github.com/newtondotcom/RaffIDE}{Code Source sur Github}}\r\n"
    			+ "\\rfoot{\\today} \r\n"
    			+ "\\cfoot{La fine équipe}\r\n"
    			+ "\\renewcommand{\\headrulewidth}{0.2pt}\r\n"
    			+ "\\renewcommand{\\footrulewidth}{0.2pt} \r\n"
    			+ "\\setlength{\\headheight}{17.5pt}\r\n"
    			+ "\r\n"
    			+ "\r\n"
    			//+"\\title{Assistant de raffinages}\r\n"
    			//+ "\\author{Equipe EF-3: AUGEREAU Robin, BELAHRACH Safae, DO COUTO VIDAL Barbara, DEORA \\\\ Axel, EL ASRI Fatima Zahra, GRAVIER Amandine, HUANG Julien, TARRADE Antonin.}\r\n"
    			//+ "\\date{\\today}\r\n"
    			+ "\\begin{document}\r\n"
    			+ "\\large \n"
    			//+"\\maketitle"
    			//+"\\newpage"
    			+ "\\begin{tabbing}\n"
    			+"{\\color{white} R0 :} \\= {\\color{white}Tant que} \\= \\\\ \r\n";
    }
    
}