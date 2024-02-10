package theAssistantDesRaffinages;

import java.util.ArrayList;
import java.util.List;

public abstract class StructureDeControle implements Element {
	
	private int elementId;
	private String condition;
	private String nom;
	private String var;
	private String debut;
	private String fin;
	private List<Element> corps;
	
	public StructureDeControle(String condition,String nom) {
		this.condition = condition;
		this.nom = nom;
		this.corps = new ArrayList<>();
	}
	
	public StructureDeControle(int id, String condition,String nom) {
		this.elementId = id;
		this.condition = condition;
		this.nom = nom;
		this.corps = new ArrayList<>();
	}
	
	public StructureDeControle(String nom,String var,String debut,String fin) {
		this.setVar(var);
		this.setDebut(debut);
		this.setFin(fin);
		this.nom = nom;
		this.corps = new ArrayList<>();
	}
	
	public StructureDeControle(String nom,int id,String var,String debut,String fin) {
		this.setVar(var);
		this.setDebut(debut);
		this.setFin(fin);
		this.elementId = id;
		this.nom = nom;
		this.corps = new ArrayList<>();
	}
	
	public void add(Element struct) {
		corps.add(struct);
	}
	
	public void delete(Element struct) {
		corps.remove(struct);
	}
	
	public void setCondition(String newCondition) {
		this.condition = newCondition;
	}
	
	public String getNom() {
	      return nom;
	}
	
	public String getCondition() {
		return condition;
	}
	
	
	public List<Element> getCorps() {
		return this.corps;
	}

	public String getVar() {
		return var;
	}

	public void setVar(String var) {
		this.var = var;
	}

	public String getDebut() {
		return debut;
	}

	public void setDebut(String debut) {
		this.debut = debut;
	}

	public String getFin() {
		return fin;
	}

	public void setFin(String fin) {
		this.fin = fin;
	}

	@Override
	public int getElementId() {
		return this.elementId;
	}
	
	@Override
	public void setElementId(int elt) {
		this.elementId = elt;
	}
	
	public String toString() {
		return "<"+ this.elementId +"s" + this.nom + ">" +
				this.toStringAbstrait()+
				"</" + this.elementId + this.nom + "s" + ">";
	}
	
	public abstract String toStringAbstrait();

}
