package theAssistantDesRaffinages;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JOptionPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

public class ActionComplexeListener implements ActionListener {

	private VueEditionRaffinages vueEdition;
	private VueListeRaffinages vueListe;

	/** Creer un observateur de l'action */
	public ActionComplexeListener(VueEditionRaffinages vueEdition,
			VueListeRaffinages vueListe) {
		this.vueListe = vueListe;
		this.vueEdition = vueEdition;

	}

	/**
	 * Action se declanchant lorsque l'on appuie sur le bouton
	 * 
	 * @param e L'evenement (appuyer sur le bouton)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		System.out.print("bouton action complexe cliqué \n");
		vueListe.getAdd().getActionListeners()[0].actionPerformed(new ActionEvent(this,1001,"add"));
	}

}
