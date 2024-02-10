package theAssistantDesRaffinages;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Map;

import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

/**
 * La classe VueListeRaffinages représente une vue contenant une JTree de
 * raffinages et un menu contextuel pour ajouter/supprimer des éléments.
 */
public class VueListeRaffinages {

	/** Le menu contextuel contenant les options Ajouter/Supprimer */
	private JPopupMenu popupMenu;
	
	/** Les éléments Ajouter et Supprimer du menu contextuel */
	private JMenuItem ajouter, supprimer;
	
	/** La JTree contenant les raffinages */
	static JTree tree;
	
	/** Les constantes pour les commandes d'ajout et de suppression */
	private static String ADD_COMMAND = "add";
	private static String DELETE_COMMAND = "remove";
	
	/** La vue d'edition (vue de gauche) */
	private VueEditionRaffinages vueEd;

	/**
	 * Constructeur de la classe. Crée un menu contextuel et y ajoute les
	 * éléments Ajouter et Supprimer. Initialise la JTree avec une racine vide.
	 */
	public VueListeRaffinages(VueEditionRaffinages vueEd) {

		this.vueEd = vueEd;
		
		// Création du menu contextuel
		popupMenu = new JPopupMenu();

		// Création des éléments Ajouter et Supprimer
		ajouter = new JMenuItem("Ajouter");
		supprimer = new JMenuItem("Supprimer");

		// Assignation des commandes pour chaque élément
		ajouter.setActionCommand(ADD_COMMAND);
		supprimer.setActionCommand(DELETE_COMMAND);

		// Ajout des éléments au menu contextuel
		popupMenu.add(ajouter);
		popupMenu.add(supprimer);

		// Création d'un observateur pour les événements de clic sur les éléments
		// du menu
		ActionListener actionListener = new PopupActionListener();
		ajouter.addActionListener(actionListener);
		supprimer.addActionListener(actionListener);

		// Initialisation de la JTree avec une racine vide
		initTree("");
	}
	
	public JMenuItem getAdd() {
		return ajouter;
	}

	public JTree getTree() {
		return tree;
	}

	public void setTree(JTree tree) {
		this.tree = tree;
	}

	/**
	 * Initialise la JTree avec la racine spécifiée.
	 * @param root La chaîne représentant la racine de la JTree.
	 */
	public void initTree(String root) {
		// Création de l'élément racine avec un ActionComplexe contenant la
		// chaîne spécifiée
		ActionComplexe raffdef = new ActionComplexe(root, 0);
		RaffinageMutableTreeNode rootNode = new RaffinageMutableTreeNode(raffdef);
		// Création de la JTree avec l'élément racine
		tree = new JTree(rootNode);

		// Assignation du menu contextuel à la JTree
		tree.setComponentPopupMenu(popupMenu);
		

		// Initialisation de l'observateur pour les événements du menu contextuel
		initPopupListener(tree, popupMenu);
		
		// Initialisation de l'observateur de click gauche de la souris
		tree.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent event) {
				if (event.getButton() == MouseEvent.BUTTON1) {
					 // Recuperation du chemin
			        TreePath path = tree.getSelectionPath();
			        
			        // Recuperation du noeud courant et parent
			        RaffinageMutableTreeNode courant = (RaffinageMutableTreeNode) path.getLastPathComponent();
			     
			        // Recuperation du raffinage courant
			        ActionComplexe raffinageCourant = (ActionComplexe) courant.getUserObject();
			        updateSurlignage((RaffinageMutableTreeNode) tree.getModel().getRoot());
			        vueEd.setRaffCourant(raffinageCourant);
			        vueEd.update();
				}
			}
		
		});
		

		// La JTree est initialement cachée
		tree.setVisible(false);
	}


	protected Color updateSurlignage(RaffinageMutableTreeNode courant) {
		int nombreEnfants = courant.getChildCount();
		Color couleur;
		ActionComplexe raffinage = (ActionComplexe) courant.getUserObject();
;		if (raffinage.estVide()) {
			couleur = Color.RED;
		} else if (nombreEnfants > 0) {
			Color couleurEnfant;
			couleur = Color.GREEN;
			for(int i=0;i<nombreEnfants;i++) {
				couleurEnfant = updateSurlignage((RaffinageMutableTreeNode) courant.getChildAt(i));
				if (!couleurEnfant.equals(Color.GREEN)) couleur = Color.ORANGE;
			}
		} else {
			couleur = Color.GREEN;
		}
		raffinage.setSurlignage(couleur);
		return couleur;
	}

	/**
	 * Changer la racine de la JTree.
	 * @param newname Le nouveau nom de la racine
	 */
	public void changeRoot(String newname) {
		
		// Creation de l'action complexe
		ActionComplexe r1 = new ActionComplexe(newname, 1,0,vueEd);

		// Recuperation de la racine
		RaffinageMutableTreeNode root = (RaffinageMutableTreeNode) tree.getModel()
				.getRoot();

		// Changement d'objet
		root.setUserObject(r1);

		// Mise a jour
		((DefaultTreeModel) tree.getModel()).nodeChanged(root);
		tree.setVisible(true);
		
		tree.getSelectionModel().setSelectionPath(new TreePath(root));
		vueEd.setRaffCourant(r1);
		vueEd.update();
	}

	/**
	 * Initialise un observateur pour le menu contextuel d'un JTree.
	 *
	 * @param tree      l'arbre auquel ajouter l'observateur
	 * @param popupMenu le menu contextuel à adapter
	 */
	private static void initPopupListener(JTree tree, JPopupMenu popupMenu) {
	    popupMenu.addPopupMenuListener(new PopupMenuListener() {
	        @Override
	        public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
	            // Obtient le rectangle du noeud selectionne
	            Rectangle rect = tree.getPathBounds(tree.getSelectionPath());

	            // Désactive les elements du menu s'il n'y a pas de noeud selectionne
	            Arrays.stream(popupMenu.getComponents())
	                    .forEach(c -> c.setEnabled(rect != null));

	            // Si aucun rectangle n'a ete obtenu, quitte la methode
	            if (rect == null) {
	                return;
	            }

	            // Convertit les coordonnees du rectangle en coordonnees d'ecran
	            Point p = new Point(rect.x + rect.width / 2, rect.y + rect.height);
	            SwingUtilities.convertPointToScreen(p, tree);

	            // Déplace le menu contextuel à la position obtenue
	            popupMenu.setLocation(p.x, p.y);
	        }

	        @Override
	        public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
	            // Ne fait rien
	        }

	        @Override
	        public void popupMenuCanceled(PopupMenuEvent e) {
	            // Ne fait rien
	        }
	    });
	}
	
	/**
     * Retourne le {@link DefaultTreeModel} correspondant à l'abre
     * @return {@link DefaultTreeModel}
     */
    public DefaultTreeModel getTreeModel() {
        return (DefaultTreeModel) this.tree.getModel();
    }
    

	/**
	 * Recuperer le ScrollPane
	 * @return un JScrollPane construit avec l'arbre
	 */
	public JScrollPane getScrollPane() {
		return new JScrollPane(this.tree);
	}

	/**
	 * Ajouter un nouveau niveau de raffinage dans l'arbre
	 * @param raffinage, le raffinage a ajouter
	 * @param precedent, le raffinage parent
	 */
	public void AddRaffinage(ActionComplexe raffinage, RaffinageMutableTreeNode precedent) {
		
		// Recuperer le modele de l'arbre
		DefaultTreeModel model = (DefaultTreeModel) tree.getModel();
		
		// Creation du nouveau noeud a partir du raffinage
		RaffinageMutableTreeNode child = new RaffinageMutableTreeNode(raffinage);
		
		// Ajout du noeud
		model.insertNodeInto(child, precedent, precedent.getChildCount());
		
		// Mise a jour du chemin
		tree.scrollPathToVisible(new TreePath(child.getPath()));
	}

	
	/**
	 * Classe interne implémentant ActionListener pour gérer les événements de clic sur les éléments du menu contextuel (popup) de la JTree.
	 */
	private class PopupActionListener implements ActionListener {
	 
	    /**
	     * Gère les événements de clic sur les éléments du menu contextuel (Ajouter/Supprimer)
	     * @param actionEvent L'événement representant un clic
	     */
	    public void actionPerformed(ActionEvent actionEvent) {
	    	
	    	// Recuperation de la commande
	        String command = actionEvent.getActionCommand();
	        
	        // Recuperation du chemin
	        TreePath path = tree.getSelectionPath();
	        
	        // Recuperation du noeud courant et parent
	        RaffinageMutableTreeNode courant = (RaffinageMutableTreeNode) path.getLastPathComponent();
	        RaffinageMutableTreeNode parent = (RaffinageMutableTreeNode) courant.getParent();
	        
	        // Recuperation du raffinage courant
	        ActionComplexe raffinageCourant = (ActionComplexe) courant.getUserObject();
	        
	        // Traitement de la commande
	        if (ADD_COMMAND.equals(command)) {
	        	String titre = "";
	        	while (titre == "") {
	        		titre = JOptionPane.showInputDialog("Entrez une action complexe");
	    		} 
	   
        		ActionComplexe action = new ActionComplexe(titre,raffinageCourant.getNiveau() + 1,vueEd.incrementerEltCourant(),vueEd);
                String ligne = vueEd.getLigneCourante();
                Element element = vueEd.getElementCourant();
                vueEd.getRaffCourant().addElement(action,ligne,element);
                vueEd.update();
                AddRaffinage(action, courant);
	        }
	        
	        if (DELETE_COMMAND.equals(command)) {
	            // Supprimer le raffinage sélectionné
	            try {
	                int nodeIndex = parent.getIndex(courant);
	                courant.removeAllChildren();
	                parent.remove(nodeIndex);
	                ActionComplexe RaffParent = (ActionComplexe) parent.getUserObject();
	                RaffParent.delElement(raffinageCourant);
			        updateSurlignage((RaffinageMutableTreeNode) tree.getModel().getRoot());
			        vueEd.setRaffCourant(RaffParent);
			        vueEd.update();
	                ((DefaultTreeModel) tree.getModel()).nodeStructureChanged((TreeNode) courant);
	            } catch (NullPointerException e) {
	                JOptionPane.showMessageDialog(supprimer, "On ne peut pas supprimer le R0");
	            }
	        }
	    }
	}	
	

}
