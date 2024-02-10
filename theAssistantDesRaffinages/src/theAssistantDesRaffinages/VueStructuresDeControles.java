package theAssistantDesRaffinages;
import java.awt.GridBagConstraints;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.GridBagLayout;
import java.util.ArrayList;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.Border;

public class VueStructuresDeControles {
	
	/** Le Panel */
	private JSplitPane structuresPanel;
	
	/** Le Label (Titre) */
	private JLabel structuresLabel;
	
	/** La liste de structure de controle */
	private List<StructureDeControle> sdcs;
	
	/** La liste de boutons*/
	private List<JButton> boutonsSdc;
	
	/** La zone d'edition */
	private VueEditionRaffinages vueEd;
	
	/**
	 * Creer un UI Structure de Controle
	 * @param sdcs La liste de structures suportees
	 */
	public VueStructuresDeControles(List<StructureDeControle> sdcs, VueEditionRaffinages vueEd) {
	
		this.vueEd = vueEd;
		
		// Initialisations des Listes
		this.sdcs = sdcs;
		this.boutonsSdc = new ArrayList<JButton>();
		
		// Initialisation du Panel
		JPanel titlePanel = new JPanel();
		JPanel boutonPanel = new JPanel(new FlowLayout());
		//structuresPanel.setLayout(new FlowLayout());
		
		// Ajout d'un Label
	    structuresLabel = new JLabel("  Structures De Controles  ");
	    Border blackBorder = BorderFactory.createLineBorder(Color.black);
	    structuresLabel.setBorder(blackBorder);
	    titlePanel.add(structuresLabel);
    
	    // Creer un Bouton pour chaque structure de controle
	    for (StructureDeControle sdc : this.sdcs) {
		   JButton newBouton = new JButton(sdc.getNom());
		   newBouton.addActionListener(new StructureDeControleListener(sdc.getClass(),vueEd));
		   boutonsSdc.add(newBouton);
		   boutonPanel.add(newBouton);
		 
	    }
	    //Creation du premier JSplitPane
        JSplitPane structuresPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT,titlePanel,boutonPanel);
        //Valeur initiale
        structuresPanel.setResizeWeight(0.05);
        this.structuresPanel = structuresPanel;
	    
 
	}
	
	/**
	 * Recuperer le Panel
	 * @return JPanel le panel
	 */
	public JSplitPane getPanel() {
		return this.structuresPanel;
	}
	
}
