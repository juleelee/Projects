package theAssistantDesRaffinages;



public class StructureSi extends StructureDeControle {

	public StructureSi(String condition,String nom) {
		super(condition,nom);
	}
	
	public StructureSi(int id,String condition,String nom) {
		super(id,condition,nom);
	}

	@Override
	public String toStringAbstrait() {
		String stringToAppend = "Si " + this.getCondition() + "  Alors :  \n" ;
		for (Element struct : this.getCorps()) {
			stringToAppend += struct.toString();
		}
		stringToAppend += "FinSi";
		return stringToAppend;		
	}
}
