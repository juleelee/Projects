package theAssistantDesRaffinages;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.nimbus.NimbusLookAndFeel;
import javax.swing.plaf.metal.*;
import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.ArrayList;
import java.util.List;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

public class Main extends JFrame {
	private static final long serialVersionUID = 1L;
	private VueEditionRaffinages vueEdition;
	private VueListeRaffinages vueListe;
	
    /**
     * 
     */
    public Main() {
    	
        // Appliquer le look and feel Nimbus
        try {
          //  UIManager.setLookAndFeel(new SystemLookAndFeel());
            UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel");
        } catch (UnsupportedLookAndFeelException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InstantiationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        
        //Zone d'edition des raffinages
        vueEdition = new VueEditionRaffinages();
        JScrollPane scrollEdition = vueEdition.getScrollPane();
        
        //Zone d'affichage de l'ensemble des raffinages
        vueListe = new VueListeRaffinages(vueEdition);
        JScrollPane scrollAffichage = vueListe.getScrollPane();
        
        
        //Créer le menu
        Menu Menuframe = new Menu(vueEdition,vueListe);
        
        //Récupérer le menu
        JMenuBar menuBarreBar = Menuframe.getJMenuBar();
        
        // Ajout de la partie structures de controles
       
        	// Creation de la liste des structures de controles
        	List<StructureDeControle> sdcs = new ArrayList<StructureDeControle>();
        	
        	// Creation de la structure de controle "Si"
        	StructureDeControle Si = new StructureSi(" ","Si");
        	sdcs.add(Si);
        	
        	// Creation de la structure de controle "TantQue"
        	StructureDeControle Tq = new StructureTantque(" ","Tant que");
        	sdcs.add(Tq);
        	
        	// Creation de la structure de controle "Repeat"
        	StructureDeControle loop = new StructureRepeat(" ","Répéter");
        	sdcs.add(loop);
        	
        	// Creation de la structure de controle "Pour"
        	StructureDeControle pour = new StructurePour("Pour"," "," "," ");
        	sdcs.add(pour);
        	
        	//Creation de la structure de controle "SiSinon"
        	StructureDeControle siSinon = new StructureSiSinon(" ","SiSinon");
        	sdcs.add(siSinon);
        	
        	// Creation de la Vue
        	VueStructuresDeControles vueStructures = new VueStructuresDeControles(sdcs,vueEdition);
        	JSplitPane structuresPanel = vueStructures.getPanel();
        	
        // Création des bouton pour les actions complexe/élémentaire
        VueAction vueActions = new VueAction(vueEdition, vueListe);
        JSplitPane actionPanel = vueActions.getPanel();
        
        //Séparation des deux fenetre action et structure de controle 
        	//Creation du premier JSplitPane
        	JSplitPane splitPanebouton = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,structuresPanel,actionPanel);
        	//Valeur initiale
        	splitPanebouton.setResizeWeight(0.5);
        
        //Creation du premier JSplitPane
        JSplitPane splitPaneLeft = new JSplitPane(JSplitPane.VERTICAL_SPLIT,scrollEdition,splitPanebouton);
        //Valeur initiale
        splitPaneLeft.setResizeWeight(0.8);
        
        
        // Création du deuxieme JSplitPane
        JSplitPane splitPaneMiddle = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, splitPaneLeft, scrollAffichage);
        
        //Valeur initiale
        splitPaneMiddle.setResizeWeight(0.5);
        
        // Ajout du conteneur à la fenêtre JFrame
        add(splitPaneMiddle);
        
        //Ajouter le menu à la fenêtre
        setJMenuBar(menuBarreBar);
        
        
        
        // Construction et injection de la barre d'outils
        JPanel contentPane = (JPanel) getContentPane();
        contentPane.add( Menuframe.createToolBar(), BorderLayout.NORTH );

        // Paramètres de la fenêtre
        setSize(1400, 800);
        setLocationRelativeTo(null);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setVisible(true);
        //setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        
        
        String r0 = JOptionPane.showInputDialog("Entrez R0");
        vueListe.changeRoot(r0);
        	
        
        
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                int choice = JOptionPane.showConfirmDialog(
                        null,
                        "Voulez-vous vraiment quitter l'application ?",
                        "Confirmation de sortie",
                        JOptionPane.YES_NO_OPTION,
                        JOptionPane.QUESTION_MESSAGE);

                if (choice == JOptionPane.YES_OPTION) {
                    // Fermer l'application
                    dispose();
                    System.exit(0);
                }
                else {
                	
                }
            }
        });
        
    }

    public VueEditionRaffinages getVueEditionRaffinages() {
		return this.vueEdition;
	}
    
    public VueListeRaffinages getVueListeRaffinages() {
		return this.vueListe;
	}
    
    public static void main(String[] args) {
        @SuppressWarnings("unused")
		Main fenetre = new Main();
    }
}
