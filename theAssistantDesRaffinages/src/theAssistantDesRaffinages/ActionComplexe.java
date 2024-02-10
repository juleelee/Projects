package theAssistantDesRaffinages;
import java.awt.Color;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
/**
 * La classe ActionComplexe implémente Action 
 * et définit les méthodes pour gérer les raffinages à décomposer(formatage, couleur..)
 */
public class ActionComplexe implements Action {
	
	private int internalActionId;
	private int elementId;
	private VueEditionRaffinages vueEd;

    /* le niveau de raffinage de l'action. */
    private int niveau;
    
    /* les sous blocs de texte de l'action complexe (par exemple les structures de controle,
     *  les actions éléméntaires la décomposant..). */
    
    private LinkedList<Integer> elements;
    private HashMap<Integer, Element> references;

    /* le titre de l'action. */
    private String titre;
    
    /* formats de texte à appliquer (gras, italique, souligné).*/
    private List<TextFormat> formats;
    
    /* la couleur du texte à appliquer. */
    private TextColor couleur;
    
    /* la couleur du surlignage */
    private Color surlignage;
    
    public ActionComplexe (String titre, int niveau) {
        this.formats = new ArrayList<>();
        this.titre = titre;
        this.couleur = TextColor.BLACK;
        this.elements = new LinkedList<>();
        this.references = new HashMap<>();
        this.niveau = niveau;
        this.surlignage = Color.RED;

    }
    
    
    public ActionComplexe (String titre, int niveau,int id) {
    	this(titre,niveau);
        this.elementId = id;
    }
    
    public ActionComplexe (String titre, int niveau,int id,VueEditionRaffinages vueEd) {
    	this(titre,niveau,id);
        this.vueEd = vueEd;
    }
    
    
//gestion des niveaux
    @Override
    public int getNiveau() {
		return niveau;
	}

	public void setNiveau(int niveau) {
		this.niveau = niveau;
	}

	public int incrementerNiveau (int niveau) {
		this.niveau += 1;
		return this.niveau;
	}
	
	public String getTitre() {
		return titre;
	}

	public void setTitre(String titre) {
		this.titre = titre;
	}

	// gestion des elements
	public void addElement (Element newElement) {
		this.references.put(newElement.getElementId(), newElement);
		this.elements.add(newElement.getElementId());
	}

	public void removeElement (Element element) {
		this.elements.remove(element.getElementId());
	}

	public LinkedList<Integer> getElements() {
		return this.elements;
	}
	
	public HashMap<Integer,Element> getReferences(){
		return this.references;
	}

//gestion du format
	
	@Override
	public void addFormat(TextFormat format) {
        this.formats.add(format);
    }
	
	@Override
	public void removeFormat(TextFormat format) {
		 this.formats.remove(format);
	}

// gestion de la couleur
	
    @Override
	public void setCouleur(TextColor nouvelleCouleur) {
        this.couleur = nouvelleCouleur;
    }
    
	@Override
	public TextColor getCouleur() {
		return this.couleur;
	}
	
   
	public void setSurlignage(Color newSurlignage) {
        this.surlignage = newSurlignage;
 
    }
    

	public Color getSurlignage() {
		return this.surlignage;
	}
    
 // affichage
    
    @Override
	public String getTexteFormate() {
        StringBuilder formattedTitle = new StringBuilder();

        if (this.couleur != TextColor.BLACK) {
        	formattedTitle.append(this.couleur.getCode());
        }

        for (TextFormat format : this.formats) {
        	formattedTitle.append(format.getCode());
        }

        formattedTitle.append(this.titre);

        if (this.couleur != TextColor.BLACK || !this.formats.isEmpty()) {
        	formattedTitle.append(TextFormat.RESET.getCode());
        }

        return formattedTitle.toString();
    }

    private String surlignageToChar(Color color) {
    	if (color.equals(Color.GREEN)){
    		return "g";
    	} else if (color.equals(Color.ORANGE)){
    		return "o";
    	} else {
    		return "r";
    	}
    }

    @Override
	public void print() {
        System.out.println(this.getTexteFormate());
    }
    
    public String afficher() {
    	String titre;
    	titre = this.getTexteFormate() + "\n";
    	return titre;
    }
    
    public String getTitreEntier() {
        return "<0t>R" + this.niveau + " : Comment " + this.titre + " ?</0t>" ;
    }

    
  
    
    @Override
    public String toString() {
    	// Initialisation du string
    	String acString ="";
    	
    	// Si on est un raffinage plus profond, on affiche juste le titre de l'Action complexe
    	if (this.getNiveau() != vueEd.getRaffCourant().getNiveau()) {
    		acString = "<" + this.elementId + "r" + surlignageToChar(this.surlignage) + ">" 
    				+ this.titre 
    				+ "</" + this.elementId + "r" + surlignageToChar(this.surlignage) + ">";
    	} else {
    		//Sinon, on affiche tout les elements contenu dans le raffinage
	    	for (Integer elementId : this.elements) {
	    		Element element = this.references.get(elementId);
	    		acString += element;
	    		
	    	} 
    	}
    	return acString;
    }
     
    
    public List<TextFormat> getFormats() {
    	return this.formats;
    }

	
	public boolean estVide() {
		return this.elements.isEmpty();
	}

	public void delElement(Element element) {
		elements.remove(element.getElementId());
		references.remove(element.getElementId());
	}

	@Override
	public int getElementId() {
		return this.elementId;
	}

	@Override
	public void setElementId(int elt) {
		this.elementId = elt;
		
	}
	
	public int getInternalActionId() {
		return this.internalActionId++;
	}
	
	public void setInternalActionId(int id) {
		this.internalActionId = id;
	}


	public void addElement(Element elementAAjouter, String ligne, Element elementClique) {
		
		if (elementClique == null) {
			addElement(elementAAjouter);
		} else {
		
			if (elementClique instanceof StructureDeControle) {
				Pattern pFin = Pattern.compile(".*(FinTq|FinSi|FinPour|Jusqu'À|FinRépéter).*");
				Matcher mFin = pFin.matcher(ligne);
				if (!mFin.find()) {
					((StructureDeControle) this.references.get(elementClique.getElementId())).add(elementAAjouter);
				} else {
					if (this.elements.size() >= this.elements.indexOf(elementClique.getElementId()+1) ) {
						this.elements.add(this.elements.indexOf(elementClique.getElementId())+1,elementAAjouter.getElementId());
					} else {
						this.elements.add(elementAAjouter.getElementId());
					}
					
				}
				
			} else {
				if (this.elements.size() >= this.elements.indexOf(elementClique.getElementId()+1) ) {
					this.elements.add(this.elements.indexOf(elementClique.getElementId())+1,elementAAjouter.getElementId());
				} else {
					this.elements.add(elementAAjouter.getElementId());
				}
			}
			this.references.put(elementAAjouter.getElementId(), elementAAjouter);
		}
		
	}

	
}
