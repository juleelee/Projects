package theAssistantDesRaffinages;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.lang.reflect.Array;
import java.util.HashSet;
import java.util.regex.*;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

public class VueEditionRaffinages {
	private HashSet<String> structKeywords;
	
	/** Zone d'édition du raffinage actif */
	JTextPane edition;
	StyledDocument doc;
	
	/** la taille de la police  ( par defaut à 14 ) */
	private int fontSize = 14;
	
	/** La police d'ecriture */
	private Font police;
	
	/** Permet de pouvoir descendre dans la colonne */
	JScrollPane scrollEdition;
	
	/** identifiant du mot en cours dans la zone d'affichage **/
	private int currentGroupId = 0;

	/** Le Raffinage courant */
	private ActionComplexe raffCourant;
	private Element elementCourantCurseur;
	private String ligneCourante;
	/**
	 * Creer un UI d'edition de raffinage
	 */
	public VueEditionRaffinages() {
		
		fontSize = 14;
		police = new Font("Monospaced", Font.PLAIN, fontSize);
		edition = new JTextPane();
		StyledDocument doc = edition.getStyledDocument();
		this.updateFontSize();
		scrollEdition = new JScrollPane(edition);
		
		edition.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                int offset = edition.viewToModel(e.getPoint());
                javax.swing.text.Element element = (javax.swing.text.Element) doc.getCharacterElement(offset);
                AttributeSet attributeSet = ((javax.swing.text.Element) element).getAttributes();
                if (attributeSet.containsAttribute("clickable", true)) {
                    String type = (String) attributeSet.getAttribute("type");
                    String mot = (String) attributeSet.getAttribute("keyword");
                    int id = (int) attributeSet.getAttribute("id");
                    System.out.println(
                            String.format("Vous avez cliqué sur '%s',\n qui est un mot-clé de type '%s',\n avec comme id %d.\n",mot,type,(id)));
                    
                    elementCourantCurseur = raffCourant.getReferences().get(id);
                    System.out.println(elementCourantCurseur);
                    
                    
                    Document doc = edition.getDocument();
                    javax.swing.text.Element root = doc.getDefaultRootElement();
                    int line = root.getElementIndex(offset);
                    javax.swing.text.Element lineElement = root.getElement(line);
                    int lineStart = lineElement.getStartOffset();
                    int lineEnd = lineElement.getEndOffset() - 1;
                    try {
						String lineText = doc.getText(lineStart, lineEnd - lineStart);
						System.out.println("Clicked on line: " + lineText);
						ligneCourante = lineText;
					} catch (BadLocationException e1) {
						e1.printStackTrace();
					}
                }
            }
        });
		
		structKeywords = new HashSet<String>();
		String[] words =  {"De","À",":","Si", "Alors", "Sinon", "FinSi",
							"SinonSi", "TantQue", "FinTantQue",
							"Répéter", "FinRépéter", "Pour","Faire", "Jusqu'À",
							"FinTQ", "FinPour"};
		for (String word:words) {
			structKeywords.add(word);
		}		
		// Permet de revenir à la ligne quand la fin de la TextArea est atteinte
		
	}
	
	public ActionComplexe getRaffCourant() {
		return this.raffCourant;
	}
	
	public void setRaffCourant(ActionComplexe raffCourant) {
		this.raffCourant = raffCourant;
	}

	public JScrollPane getScrollPane() {
		return this.scrollEdition;
	}

	/**
	 * decremente la taille de la police
	 * 
	 * @param decrement de combien faut-il diminuer la taille
	 */
	public void decreaseFontSize(int decrement) {
		if (this.fontSize - decrement > 1) {
			this.fontSize -= decrement;
		}
		this.updateFontSize();
	}
	/*
	 * Lors d'un click decremente la taille de la police de 0,5 
	 */
	public void decFontSizeOnClick() {
		if (this.fontSize >= 1) {
			this.fontSize--;
			this.updateFontSize();
		}
	}
	
	/*
	 * Lors d'un click augmente la taille de la police de 0,5 
	 */
	public void incFontSizeOnClick() {

		this.fontSize++;
		this.updateFontSize();
		
	}

	/**
	 * incremente la taille de la police
	 * 
	 * @param increment de combien faut-il augmenter la taille
	 */
	public void increaseFontSize(int increment) {
		if (this.fontSize + increment < 40) {
			this.fontSize += increment;
		}
		this.updateFontSize();
	}

	public void updateFontSize() {
		this.police = new Font("Monospaced", Font.PLAIN, fontSize);
		edition.setFont(police);
	}
	
	public void updateFontSize(int new_size) {
		this.police = new Font("Monospaced", Font.PLAIN, new_size);
		edition.setFont(police);
	}
	
	public JTextPane getTextArea() {
		return this.edition;
	}
	
	public void updatePolice(Font newPolice) {
		this.police = newPolice;
		edition.setFont(police);
	}
	
	public Font getPolice() {
		return this.police;
	}
	
	public int getFontSize() {
		return this.fontSize;
	}
	
	public void setFontSize(int newSize) {
		this.fontSize = newSize;
	}

	/**
	 * Ajoute un String à la zone de texte.
	 * @param stringToAppend
	 */
	public void append(String stringToAppend) {
		System.out.println("Append : " + stringToAppend);
		try{
			
		doc = edition.getStyledDocument();

		
		String mot = "";
		String type = "";
		String regex = "<([0-9]+)([a-z])([a-zA-Z]*)>(.*)</\\1\\2(\\3*)>";
		
		Pattern p = Pattern.compile(regex,Pattern.DOTALL);
		Matcher m = p.matcher(stringToAppend);
		
		while(m.find()) {	
			
			System.out.println("Match!");
			
			mot = m.group(4);
			System.out.println("Mot : " + mot);
			
			switch (m.group(2)) {
			
			case "r": 
				type = "raffinage";
				switch (m.group(3)) {
				
				case "r":
					type += "R";
					break;
					
				case "o":
					type += "O";
					break;
					
				default:
					type += "V";
					break;
				}
			break;
			
			case "s":
				System.out.println(mot.split(":")[0]+mot.split(":")[1].replaceAll(regex, ""));
				type = "structure";
				String motSplit[] = mot.split("\n");
				doc.insertString(doc.getLength(),motSplit[0] + "\n",
						createStyle(mot, type, currentGroupId++));
				
				append(m.group(4));
				
				doc.insertString(doc.getLength(),motSplit[1].replaceAll(regex, "") + "\n",
						createStyle(mot, type, currentGroupId));
				break;
				
			case "t":
				type = "titre";
				break;
					
			default:
				type = "elementaire";
				break;
			}
			
			if (type != "structure" && mot != "") {
				doc.insertString(doc.getLength(),
				mot + "\n",
				createStyle(mot, type, currentGroupId++));
			}
			
		}
		if (mot == "") {
			System.out.println("NO MATCHES");
		}
		
		
		
		
		
		
	
//		String[] mots = stringToAppend.split(" ");
		
//		for (String mot:mots) {
//			System.out.println(mot);
//			String type; 
//				if (this.structKeywords.contains(mot.replaceAll("[\n\t]", ""))) {
//					type = "structure";
//					
//				}else if (mot.contains("<r")) {
//
//					type = "raffinage";
//					
//					if (mot.contains("<rg>")) {
//						type += 'G';
//						mot = mot.replaceAll("<rg>", "");
//					} else if (mot.contains("<ro>")) {
//						type += 'O';
//						mot = mot.replaceAll("<ro>", "");
//					} else {
//						type += 'R';
//						mot = mot.replaceAll("<rr>", "");
//					}
//					mot = mot.replace("<s>", " ");
//					
//				} else if (mot.contains("<t>")) {
//					type = "titre";
//					mot = mot.replaceAll("<t>","");
//					mot = mot.replaceAll("<s>"," ");
//				} else {
//					type = "condition";
//				}
//			
//			doc.insertString(doc.getLength(),
//					mot+(mot.contains("\n")?"":" "),
//					createStyle(mot, type, currentGroupId++));
//		}
 
	
		
		
		}catch (Exception e) {
			e.printStackTrace();
		}
		
		
		
		
	}
	
	private SimpleAttributeSet createStyle(String mot, String type, int id) {
        SimpleAttributeSet style = new SimpleAttributeSet();
        style.addAttribute("clickable", true);
        switch(type) {
        case "structure":
        	style.addAttribute(StyleConstants.Foreground, Color.RED);
            style.addAttribute("emphasis", TextAttribute.WEIGHT_BOLD);
            break;
        case "raffinageR":
        	style.addAttribute(StyleConstants.Background, Color.RED);
        	break;
        case "raffinageV":
        	style.addAttribute(StyleConstants.Background, Color.GREEN);
        	break;
        case "raffinageO":
        	style.addAttribute(StyleConstants.Background, Color.ORANGE);
        	break;
        
        case "titre":
        	style.addAttribute(StyleConstants.Foreground, Color.BLUE);
        	
        default:
        	style.addAttribute(StyleConstants.Foreground, Color.BLUE);
            style.addAttribute("emphasis", TextAttribute.WEIGHT_LIGHT);
            break;
        }
        
        style.addAttribute("keyword", mot);
        style.addAttribute("type", type);
        style.addAttribute("id", id);
        return style;
    }

	public void update() {
		currentGroupId = 0;
		ligneCourante = "";
		elementCourantCurseur = null;
		edition.setText("");
		this.append(raffCourant.getTitreEntier() + raffCourant.toString());
	}
	
	public int incrementerEltCourant() {
		return this.currentGroupId++;
	}

	public String getLigneCourante() {
		return ligneCourante;
	}
	
	public Element getElementCourant() {
		return this.elementCourantCurseur;
	}
	
}
