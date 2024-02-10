package theAssistantDesRaffinages;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.InvocationTargetException;

import javax.swing.JOptionPane;
import javax.swing.JTextArea;

public class StructureDeControleListener implements ActionListener {

	/** La structure de controle a observer */
	private Class<? extends StructureDeControle> classe;

	/** La zone d'edition */
	private VueEditionRaffinages vueEd;

	/** Creer un observateur de structure de controle */
	public StructureDeControleListener(
			Class<? extends StructureDeControle> class1,
			VueEditionRaffinages vueEd) {
		this.classe = class1;
		this.vueEd = vueEd;
	}

	/**
	 * Action se declanchant lorsque l'on appuie sur le bouton
	 * 
	 * @param e L'evenement (appuyer sur le bouton)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		//Initialisation de la Structure
		StructureDeControle sdc;
		
		// Traitement special de la structure Pour
		if (classe == StructurePour.class) {
			String var = JOptionPane
					.showInputDialog("Entrez un nom de variable");
			String Debut = JOptionPane
					.showInputDialog("Entrez le début de la boucle");
			String Fin = JOptionPane
					.showInputDialog("Entrez la fin de la boucle");
			System.out.println(e.getSource().getClass());
			
			// Instanciation d'une nouvelle structure Pour a partir des donnees entrees par l'utilisateur
			try {
				sdc = classe
						.getConstructor(new Class<?>[] { String.class,int.class,
								String.class, String.class, String.class })
						.newInstance("",vueEd.incrementerEltCourant(),var, Debut, Fin);
				
				String ligne = vueEd.getLigneCourante();
                Element element = vueEd.getElementCourant();
				vueEd.getRaffCourant().addElement(sdc,ligne,element);
				
				System.out.println(sdc.getElementId());
			} catch (InstantiationException | IllegalAccessException
					| IllegalArgumentException | InvocationTargetException
					| NoSuchMethodException | SecurityException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		}

		// Pour toutes les autre structures
		else {
			String condition = JOptionPane
					.showInputDialog("Entrez une condition");
			// Instanciation generique de la structure 
			try {
				sdc = classe
						.getConstructor(
								new Class<?>[] { int.class,String.class, String.class })
						.newInstance(vueEd.incrementerEltCourant(),condition, "");
				
				vueEd.getRaffCourant().addElement(sdc);
			} catch (InstantiationException | IllegalAccessException
					| IllegalArgumentException | InvocationTargetException
					| NoSuchMethodException | SecurityException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		}
		// Mise a jour de la vue d'Edition des raffinage
		vueEd.update();
	}

}