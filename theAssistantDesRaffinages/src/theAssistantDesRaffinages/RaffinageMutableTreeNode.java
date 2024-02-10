package theAssistantDesRaffinages;


import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.tree.DefaultMutableTreeNode;

public class RaffinageMutableTreeNode extends DefaultMutableTreeNode {
	
	public RaffinageMutableTreeNode(ActionComplexe raffinage){
		super(raffinage);
	}
	
	@Override
	public String toString() {
		Pattern pTitre = Pattern.compile("<(0t)>(.*)</\\1>"); 
		Matcher mTitre = pTitre.matcher(((ActionComplexe) this.getUserObject()).getTitreEntier());
		mTitre.find();
		return mTitre.group(2);
		
	}

	public ActionComplexe getActionComplexe() {
		return ((ActionComplexe) this.getUserObject());
	}
}
