package theAssistantDesRaffinages;

public class StructurePour extends StructureDeControle {

	public StructurePour(String nom, String var, String debut, String fin) {
		super(nom, var, debut, fin);
	}
	
	public StructurePour(String nom, int id, String var, String debut, String fin) {
		super(nom,id, var, debut, fin);
	}

	@Override
	public String toStringAbstrait(){
		String stringToAppend = "Pour " + this.getVar() + " De " + this.getDebut() + " À " + this.getFin() + " Faire : \n" ;
		for (Element struct : this.getCorps()) {
			stringToAppend += struct;
		}
		stringToAppend += "FinPour";
		return stringToAppend; }
}