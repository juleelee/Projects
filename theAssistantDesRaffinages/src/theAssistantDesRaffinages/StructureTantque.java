package theAssistantDesRaffinages;

public class StructureTantque extends StructureDeControle {
	public StructureTantque(String condition,String nom) {
		super(condition,nom);
	}
	
	public StructureTantque(int id,String condition,String nom) {
		super(id,condition,nom);
	}

	@Override
	public String toStringAbstrait() {
		String stringToAppend = "TantQue  " + this.getCondition() + " Faire : \n" ;
		for (Element struct : this.getCorps()) {
			stringToAppend += struct;
		}
		stringToAppend += "FinTQ";
		return stringToAppend;
		
	}
}


