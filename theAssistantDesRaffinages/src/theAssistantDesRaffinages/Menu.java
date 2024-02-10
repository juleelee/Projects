package theAssistantDesRaffinages;

import java.awt.BorderLayout;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JComboBox;
import javax.swing.JLabel;

import javax.swing.JOptionPane;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.KeyStroke;

import javax.swing.JPanel;
import javax.swing.JTextPane;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;
import javax.swing.tree.DefaultTreeModel;

import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.text.Style;

import theAssistantDesRaffinages.Export;

@SuppressWarnings("serial")
public class Menu extends JFrame {

    private static final long serialVersionUID = 1L;
    private VueEditionRaffinages vueEdition;
    private VueListeRaffinages vueListe;
    private String filePath;
    
    private boolean italique = false; // initialisation de la variable italique
    
    private JLabel fontSizeLabel;
    
	/* Construction de l'interface graphique */
    public Menu(VueEditionRaffinages vueEdition, VueListeRaffinages vueListe) {
        super( "Ra77ineur" );
        this.vueEdition = vueEdition;
        this.vueListe = vueListe;
        this.setSize(600,400);
        this.setLocationRelativeTo( null );
        this.setDefaultCloseOperation( DISPOSE_ON_CLOSE );
        
   
        // Construction et injection de la barre de menu
        this.setJMenuBar( this.createMenuBar() );

        // Construction et injection de la barre d'outils
        JPanel contentPane = (JPanel) getContentPane();
        contentPane.add( this.createToolBar(), BorderLayout.NORTH );
    }

    /* Methode de construction de la barre de menu */
    public JMenuBar createMenuBar() {

        // La barre de menu à proprement parler
        JMenuBar menuBar = new JMenuBar();

        // Définition du menu déroulant "File" et de son contenu
        JMenu mnuFile = new JMenu( "File" );
        mnuFile.setMnemonic( 'F' );

        /*JMenuItem mnuNewFile =*/ 
        mnuFile.add( actNew );
        mnuFile.addSeparator();
        mnuFile.add( actOpen );
        mnuFile.add( actSave );
        mnuFile.add( actSaveAs );
        mnuFile.addSeparator();
        mnuFile.add( actExit );
       
        
        menuBar.add(mnuFile);
        
        // Définition du menu déroulant "Edit" et de son contenu
        JMenu mnuEdit = new JMenu( "Edit" );
        mnuEdit.setMnemonic( 'E' );
        
        mnuEdit.add( actUndo );
        mnuEdit.add( actRedo );
        mnuEdit.addSeparator();
        mnuEdit.add( actCopy );
        mnuEdit.add( actCut );
        mnuEdit.add( actPaste );

        menuBar.add(mnuEdit);

        // Définition du menu déroulant "Help" et de son contenu
        JMenu mnuHelp = new JMenu("Help");
        mnuHelp.setMnemonic('H');

        JMenuItem menuItem = new JMenuItem("Contact");
        menuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                String githubLink = "https://github.com/newtondotcom/theAssistantDesRaffinages";

                // Copier le lien dans le presse-papiers
                StringSelection stringSelection = new StringSelection(githubLink);
                Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
                clipboard.setContents(stringSelection, null);

                JOptionPane.showMessageDialog(null, "Le lien GitHub a été copié dans le presse-papiers : " + githubLink);
            }
        });

        mnuHelp.add(menuItem);
        menuBar.add(mnuHelp);
        
        return menuBar;
    }

    /* Methode de construction de la barre d'outils */
    public JToolBar createToolBar() {
        JToolBar toolBar = new JToolBar();

        toolBar.add( actNew ).setHideActionText( true );
        toolBar.addSeparator();
        toolBar.add( actOpen ).setHideActionText( true );
        toolBar.add( actSave ).setHideActionText( true );
        toolBar.add( actSaveAs ).setHideActionText( true );
        
        
       

        /* Ajout pour pouvoir changer police d'ecriture */
        
        // Créer une liste de noms de police disponibles
        String[] fonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();

        // Créer la JComboBox pour sélectionner une police
        JComboBox<String> police = new JComboBox<>(fonts);
        police.setMaximumSize(new Dimension(150, police.getPreferredSize().height)); // ajuster la taille de la JComboBox
        police.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                String fontName = (String) police.getSelectedItem();
                Font font = new Font(fontName, Font.PLAIN, vueEdition.getFontSize());
                vueEdition.updatePolice(font);
            }
        });

        // Ajouter la JComboBox à la JToolBar
        toolBar.add(police);

        
     // Créer un JLabel pour afficher la taille de police courante
        
        toolBar.add(incFontSizeButton).setHideActionText(true);;
        toolBar.add(decFontSizeButton).setHideActionText(true);;
        //toolBar.add(fontSizeLabel);
        
        

     
        toolBar.add(actBold).setHideActionText(true);
        toolBar.add(actItalic).setHideActionText(true);
        toolBar.add(actUnderline).setHideActionText(true);
        toolBar.add(actZoomIn).setHideActionText(true);
        toolBar.add(actZoomOut).setHideActionText(true);
        toolBar.addSeparator();
        toolBar.add( actExit ).setHideActionText( true );
       
        return toolBar;
    }

    
    
    /* Nos diverses actions */
    private AbstractAction actNew = new AbstractAction() {  
        {
            putValue( Action.NAME, "New File..." );
            putValue( Action.SMALL_ICON, new ImageIcon( "icons/new.png" ) );
            putValue( Action.MNEMONIC_KEY, KeyEvent.VK_N );
            putValue( Action.SHORT_DESCRIPTION, "New file (CTRL+N)" );
            putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_DOWN_MASK ) ); 
        }
        
        @Override public void actionPerformed( ActionEvent e ) {
            System.out.println( "New" );
            Main nouvelleFenetre = new Main();
            nouvelleFenetre.setVisible(true);
            //vueEdition = new VueEditionRaffinages();
            //vueListe = new VueListeRaffinages(vueEdition);
        }
    };

    private AbstractAction actOpen = new AbstractAction() {  
        {
            putValue( Action.NAME, "Open File..." );
            putValue( Action.SMALL_ICON, new ImageIcon( "icons/open.png" ) );
            putValue( Action.MNEMONIC_KEY, KeyEvent.VK_O );
            putValue( Action.SHORT_DESCRIPTION, "Open file (CTRL+O)" );
            putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_O, KeyEvent.CTRL_DOWN_MASK ) ); 
        }
        
        @Override public void actionPerformed( ActionEvent e ) {
            System.out.println( "Open" );
            //Ouvrir le fichier et modifier l'arbre correspondant
        }
    };

    private AbstractAction actSave = new AbstractAction() {  
        {
            putValue( Action.NAME, "Save File" );
            putValue( Action.SMALL_ICON, new ImageIcon( "icons/save.png" ) );
            putValue( Action.MNEMONIC_KEY, KeyEvent.VK_S );
            putValue( Action.SHORT_DESCRIPTION, "Save file (CTRL+S)" );
            putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S, KeyEvent.CTRL_DOWN_MASK ) ); 
        }
        
        @Override public void actionPerformed( ActionEvent e ) {
            System.out.println( "Save" );
            //Rajouter enregistrement normal
            ///Pas implémenté : Utilisation de l'export
            // Créer un objet JFileChooser pour permettre à l'utilisateur de sélectionner un emplacement et un nom de fichier
            JFileChooser fileChooser = new JFileChooser();
            fileChooser.setDialogTitle("Save as"); // Titre de la boîte de dialogue
            fileChooser.setFileFilter(new FileNameExtensionFilter("Tex Files", "tex")); // Filtrer les fichiers pour afficher uniquement les fichiers .json
            
            // Afficher la boîte de dialogue pour que l'utilisateur sélectionne l'emplacement et le nom du fichier à enregistrer
            int userSelection = fileChooser.showSaveDialog(null); 
            if (userSelection == JFileChooser.APPROVE_OPTION) {
                // Si l'utilisateur a sélectionné un emplacement et un nom de fichier, récupérer le fichier sélectionné
                File fileToSave = fileChooser.getSelectedFile();
                //String filePath = fileToSave.getAbsolutePath();
                filePath = fileToSave.getPath();
                if (!filePath.endsWith(".tex")) {
                    // Si le nom de fichier ne se termine pas par .txt, ajouter l'extension .txt
                    filePath = filePath + ".tex";
                }
            }
            //VueListeRaffinages vueListe = VueListeRaffinages.get();
            JTree arbre = vueListe.getTree();
            Export classExport = new Export();
            String dataString = classExport.getString(arbre);

            FileWriter writer;
			try {
				writer = new FileWriter(filePath);
				writer.write(dataString);
				writer.close();
			} catch (IOException e1) {
				e1.printStackTrace();
			}
            
        }
    };
   
    private AbstractAction actSaveAs = new AbstractAction() {  
        {
            putValue( Action.NAME, "Export As..." );
            putValue( Action.SMALL_ICON, new ImageIcon( "icons/save_as.png" ) );
            putValue( Action.MNEMONIC_KEY, KeyEvent.VK_A );
            putValue( Action.SHORT_DESCRIPTION, "Export file in .tex" );
        }
        
        @Override public void actionPerformed( ActionEvent e ) {
            System.out.println( "Export as" );

            // Créer un objet JFileChooser pour permettre à l'utilisateur de sélectionner un emplacement et un nom de fichier
            JFileChooser fileChooser = new JFileChooser();
            fileChooser.setDialogTitle("Save as"); // Titre de la boîte de dialogue
            fileChooser.setFileFilter(new FileNameExtensionFilter("Tex Files", "tex")); // Filtrer les fichiers pour afficher uniquement les fichiers .json
            
            // Afficher la boîte de dialogue pour que l'utilisateur sélectionne l'emplacement et le nom du fichier à enregistrer
            int userSelection = fileChooser.showSaveDialog(null); 
            if (userSelection == JFileChooser.APPROVE_OPTION) {
                // Si l'utilisateur a sélectionné un emplacement et un nom de fichier, récupérer le fichier sélectionné
                File fileToSave = fileChooser.getSelectedFile();
                //String filePath = fileToSave.getAbsolutePath();
                filePath = fileToSave.getPath();
                if (!filePath.endsWith(".tex")) {
                    // Si le nom de fichier ne se termine pas par .txt, ajouter l'extension .txt
                    filePath = filePath + ".tex";
                }
            }
            //VueListeRaffinages vueListe = VueListeRaffinages.get();
            JTree arbre = vueListe.getTree();
            Export classExport = new Export();
            String dataString = classExport.getString(arbre);

            FileWriter writer;
			try {
				writer = new FileWriter(filePath);
				writer.write(dataString);
				writer.close();
			} catch (IOException e1) {
				e1.printStackTrace();
			}

		}
	};

	private AbstractAction actExit = new AbstractAction() {
		{
			putValue(Action.NAME, "Exit");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/exit.png"));
			putValue(Action.MNEMONIC_KEY, KeyEvent.VK_X);
			putValue(Action.SHORT_DESCRIPTION, "Exit (ALT+F4)");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F4, KeyEvent.ALT_DOWN_MASK));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
	        int choice = JOptionPane.showConfirmDialog(null, "Voulez vous vraiment quitter ? (il faut sauvegarder dans le vie)", "Exit Confirmation",
	                JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
	        if (choice == JOptionPane.YES_OPTION) {
	            System.exit(0);
	        }
	        else {
	        	
	        }
		}
	};

	private AbstractAction actUndo = new AbstractAction() {
		{
			putValue(Action.NAME, "Undo");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/undo.png"));
			putValue(Action.MNEMONIC_KEY, KeyEvent.VK_U);
			putValue(Action.SHORT_DESCRIPTION, "Undo (CTRL+Z)");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Z, KeyEvent.CTRL_DOWN_MASK));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			System.out.println("Undo");
		}
	};

	private AbstractAction actRedo = new AbstractAction() {
		{
			putValue(Action.NAME, "Redo");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/redo.png"));
			putValue(Action.MNEMONIC_KEY, KeyEvent.VK_R);
			putValue(Action.SHORT_DESCRIPTION, "Redo (CTRL+U)");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_U, KeyEvent.CTRL_DOWN_MASK));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			System.out.println("Redo");
		}
	};

	private AbstractAction actZoomIn = new AbstractAction() {
		{
			putValue(Action.NAME, "Bigger");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/ZoomIn.png"));
			putValue(Action.MNEMONIC_KEY, KeyEvent.VK_C);
			putValue(Action.SHORT_DESCRIPTION, "Zoom In");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_C, KeyEvent.VK_PLUS));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			vueEdition.increaseFontSize(2);
		}
	};

	private AbstractAction actZoomOut = new AbstractAction() {
		{
			putValue(Action.NAME, "Smaller");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/ZoomOut.png"));
			putValue(Action.MNEMONIC_KEY, KeyEvent.VK_C);
			putValue(Action.SHORT_DESCRIPTION, "Zoom Out");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_C, KeyEvent.VK_MINUS));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			vueEdition.decreaseFontSize(2);
		}
	};

	private AbstractAction actCopy = new AbstractAction() {
		{
			putValue(Action.NAME, "Copy");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/copy.png"));
			putValue(Action.MNEMONIC_KEY, KeyEvent.VK_C);
			putValue(Action.SHORT_DESCRIPTION, "Copy (CTRL+C)");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_C, KeyEvent.CTRL_DOWN_MASK));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			System.out.println("Copy");
		}
	};

	private AbstractAction actCut = new AbstractAction() {
		{
			putValue(Action.NAME, "Cut");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/cut.png"));
			putValue(Action.MNEMONIC_KEY, KeyEvent.VK_T);
			putValue(Action.SHORT_DESCRIPTION, "Cut (CTRL+X)");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_X, KeyEvent.CTRL_DOWN_MASK));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			System.out.println("Cut");
		}
	};

	private AbstractAction actPaste = new AbstractAction() {
		{
			putValue(Action.NAME, "Paste");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/paste.png"));
			putValue(Action.MNEMONIC_KEY, KeyEvent.VK_P);
			putValue(Action.SHORT_DESCRIPTION, "Paste (CTRL+V)");
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_V, KeyEvent.CTRL_DOWN_MASK));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			System.out.println("Paste");
		}

	};

	// Déclaration de la variable pour suivre l'état actuel de l'italique
	private boolean estEnItalique = false;

	private AbstractAction actItalic = new AbstractAction() {
		{
			putValue(Action.NAME, "Italic");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/italic.png"));

		}

		@Override
		public void actionPerformed(ActionEvent e) {
			System.out.println("Italique");

			// Création d'un style en italique
			SimpleAttributeSet italic = new SimpleAttributeSet();
			StyleConstants.setItalic(italic, !estEnItalique);

			// Application du style en italique au texte sélectionné
			StyledDocument doc = vueEdition.getTextArea().getStyledDocument();
			int debutSelection = vueEdition.getTextArea().getSelectionStart();
			int finSelection = vueEdition.getTextArea().getSelectionEnd();
			doc.setCharacterAttributes(debutSelection, finSelection - debutSelection, italic, false);

			// Mise à jour de la variable estEnItalique
			estEnItalique = !estEnItalique;
		}

	};

	private boolean estSouligne = false;

	private AbstractAction actUnderline = new AbstractAction() {
		{
			putValue(Action.NAME, "Underline");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/underline.png"));

		}

		@Override
		public void actionPerformed(ActionEvent e) {
			System.out.println("Underline");

			JTextPane textPane = vueEdition.getTextArea();

			int debut = textPane.getSelectionStart();
			int fin = textPane.getSelectionEnd();

			StyledDocument doc = textPane.getStyledDocument();
			Style style = doc.addStyle("souligne", null);

			// Si le texte est déjà souligné, on l'enlève
			if (estSouligne) {
				StyleConstants.setUnderline(style, false);
				estSouligne = false;
			} else { // Sinon, on ajoute le soulignement
				StyleConstants.setUnderline(style, true);
				estSouligne = true;
			}

			doc.setCharacterAttributes(debut, fin - debut, style, false);
		}

	};

	private boolean estGras = false;

	private AbstractAction actBold = new AbstractAction() {
		{
			putValue(Action.NAME, "Bold");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/bold.png"));

		}

		@Override
		public void actionPerformed(ActionEvent e) {
			System.out.println("Bold");

			// obtenir le texte sélectionné dans le JTextPane
			JTextPane textPane = vueEdition.getTextArea();

			StyledDocument doc = textPane.getStyledDocument();
			int debutSelection = textPane.getSelectionStart();
			int finSelection = textPane.getSelectionEnd();

			// si le texte est déjà en gras, on le met en normal
			if (estGras) {
				StyleConstants.setBold(doc.getStyle(StyleContext.DEFAULT_STYLE), false);
				doc.setCharacterAttributes(debutSelection, finSelection - debutSelection,
						doc.getStyle(StyleContext.DEFAULT_STYLE), true);
				estGras = false;
			} else {
				// sinon, on le met en gras
				Style style = doc.addStyle("gras", null);
				StyleConstants.setBold(style, true);
				doc.setCharacterAttributes(debutSelection, finSelection - debutSelection, style, true);
				estGras = true;
			}
		}

	};

	private AbstractAction actPoliceTimes = new AbstractAction() {
		{
			putValue(Action.NAME, "Times New Roman");

		}

		@Override
		public void actionPerformed(ActionEvent e) {
			int taille = vueEdition.getFontSize();
			Font police = new Font("Times New Roman", Font.PLAIN, taille);
			vueEdition.updatePolice(police);
		}

	};

	private AbstractAction actPoliceArial = new AbstractAction() {
		{
			putValue(Action.NAME, "Arial");

		}

		@Override
		public void actionPerformed(ActionEvent e) {
			int taille = vueEdition.getFontSize();
			Font police = new Font("Arial", Font.PLAIN, taille);
			vueEdition.updatePolice(police);
		}

	};

	private AbstractAction actPoliceCourier = new AbstractAction() {
		{
			putValue(Action.NAME, "Courier New");

		}

		@Override
		public void actionPerformed(ActionEvent e) {
			int taille = vueEdition.getFontSize();
			Font police = new Font("Courier New", Font.PLAIN, taille);
			vueEdition.updatePolice(police);
		}

	};

	private AbstractAction incFontSizeButton = new AbstractAction() {
		{
			putValue(Action.NAME, "+");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/plus.png"));

		}

		@Override
		public void actionPerformed(ActionEvent e) {
			vueEdition.incFontSizeOnClick();
			fontSizeLabel = new JLabel("Taille : " + vueEdition.getFontSize());
		}

	};

	private AbstractAction decFontSizeButton = new AbstractAction() {
		{
			putValue(Action.NAME, "-");
			putValue(Action.SMALL_ICON, new ImageIcon("icons/minus.png"));
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			vueEdition.decFontSizeOnClick();
			fontSizeLabel = new JLabel("Taille : " + vueEdition.getFontSize());
		}

	};

	public void mnuNewListener(ActionEvent event) {
		JOptionPane.showMessageDialog(this, "Button clicked !");
	}
}