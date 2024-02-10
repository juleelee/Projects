package theAssistantDesRaffinages;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JOptionPane;

public class ActionElementaireListener implements ActionListener {
	
	/** L'action élémentaire */
	//private ActionElementaire action;
	private VueEditionRaffinages vueEd;

	/** Creer un observateur de l'action */
	public ActionElementaireListener(VueEditionRaffinages vueEdition) {
		//this.action = new ActionElementaire(vueEdition);
		this.vueEd = vueEdition;
	}
	
	/**
	 * Action se declanchant lorsque l'on appuie sur le bouton
	 * @param e L'evenement (appuyer sur le bouton)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		System.out.print("bouton action élémentaire clické \n");
		
		String titre = null;
		while (titre == null) {
			titre = JOptionPane.showInputDialog("Entrez une action élémentaire");
		}
		
		ActionElementaire action = new ActionElementaire(vueEd);
		action.setTitre(titre);
		action.setElementId(vueEd.incrementerEltCourant());
		
		String ligne = vueEd.getLigneCourante();
        Element element = vueEd.getElementCourant();
        vueEd.getRaffCourant().addElement(action,ligne,element);
        this.vueEd.update();
		
		
	}

}
