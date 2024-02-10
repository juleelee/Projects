package theAssistantDesRaffinages;
import java.util.ArrayList;
import java.util.List;
/**
 * La classe ActionElementaire implémente Action 
 * et définit les méthodes pour gérer les actions simples qui ne seront pas à raffiner après.
 */

public class ActionElementaire implements Action {

		private int elementId;
		/* le titre de l'action. */
	    private String titre;
	    /* le niveau de raffinage de l'action. */
	    private int niveau;
	    /* formats de texte à appliquer (gras, italique, souligné).*/
	    private List<TextFormat> formats;
	    /* la couleur du texte à appliquer. */
	    private TextColor couleur;	    
	    private VueEditionRaffinages aireTexte;

	    public ActionElementaire (VueEditionRaffinages textArea) {
	        this.formats = new ArrayList<>();
	        this.couleur = TextColor.BLACK;
	        this.aireTexte = textArea;

	    }
	    
	    public ActionElementaire () {
	        this.formats = new ArrayList<>();
	        this.couleur = TextColor.BLACK;
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
		
		

		public String getTitre() {
			return titre;
		}

		public void setTitre(String nouveauTitre) {
			this.titre = nouveauTitre;
		}

		// gestion du niveau
		@Override
		public int getNiveau() {
	        return this.niveau;
	    }
		
		public void setNiveau(int niveau) {
			this.niveau = niveau;
		}

		public int incrementerNiveau (int niveau) {
			this.niveau += 1;
			return this.niveau;
		}

	  /*  @Override
	    public void setContent(String content) {
	        this.textArea.setText(content);
	    }
	   */
	    
// gestion du format
	    
	    @Override
		public void addFormat(TextFormat format) {
	        this.formats.add(format);
	    }
	    
		@Override
		public void removeFormat(TextFormat format) {
			 this.formats.remove(format);
		}
		
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
	   
// affichage

	    @Override
		public void print() {
	        System.out.println(this.getTexteFormate());
	    }
	    
	    @Override
	    public String toString() {
	    	return "<" + this.elementId + "e>" +this.titre + "</"+ this.elementId + "e>"; 
	    }

		@Override
		public int getElementId() {
			return this.elementId;
		}

		@Override
		public void setElementId(int elt) {
			this.elementId = elt;
			
		}

	    

}
