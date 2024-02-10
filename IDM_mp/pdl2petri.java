package simplepdl.manip;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;

import petrinet.*;

import simplepdl.GestRessource;
import simplepdl.Process;
import simplepdl.Ressource;
import simplepdl.SimplepdlPackage;
import simplepdl.WorkDefinition;
import simplepdl.WorkSequence;

public class pdl2petri {

	static PetrinetFactory pnFactory;
	static PetriNet petrinet;
    
    static Map<String, Place> begin = new HashMap<String, Place>();
	static Map<String, Place> started1 = new HashMap<String, Place>();
	static Map<String, Place> started2 = new HashMap<String, Place>();
	static Map<String, Place> end = new HashMap<String, Place>();
	static Map<String, Place> ressources = new HashMap<String, Place>();
    static Map<String, Transition> start = new HashMap<String, Transition>();
    static Map<String, Transition> finish = new HashMap<String, Transition>();
	

	// Convertir les WorkDefinitions
	private static void ConvertWD(WorkDefinition wd) {
		/*Création des places */
		
		// création de la premiere place : begin 
		Place begin_p = pnFactory.createPlace();
        begin_p.setName("begin_" + wd.getName());
        begin_p.setNb_jetons(1);
        petrinet.getPetrinetelement().add(begin_p);
		begin.put(wd.getName(), begin_p);

		//création de la deuxieme place une fois commencé : started1
		Place started1_p = pnFactory.createPlace();
		started1_p.setName("started1_" + wd.getName());
		started1_p.setNb_jetons(0);
		petrinet.getPetrinetelement().add(started1_p);
		started1.put(wd.getName(), started1_p);

		// création de place en parallèle pour indiquer que l'activité a commencé : started2
		Place started2_p = pnFactory.createPlace();
		started2_p.setName("started2_" + wd.getName());
		started2_p.setNb_jetons(0);
		petrinet.getPetrinetelement().add(started2_p);
		started2.put(wd.getName(), started2_p);

		// création de la place : end
		Place end_p = pnFactory.createPlace();
		end_p.setName("end_" + wd.getName());
		end_p.setNb_jetons(0);
		petrinet.getPetrinetelement().add(end_p);
		end.put(wd.getName(), end_p);


		/*Création des transitions */

		// création de la transition : start 
		Transition TransitionStart = pnFactory.createTransition();
		TransitionStart.setName("start_" + wd.getName());
		petrinet.getPetrinetelement().add(TransitionStart);
		start.put(wd.getName(), TransitionStart);

		// création de la transition  : finish
		Transition TransitionFinish = pnFactory.createTransition();
		TransitionFinish.setName("finish_" + wd.getName());
		petrinet.getPetrinetelement().add(TransitionFinish);
		finish.put(wd.getName(), TransitionFinish);

		/*Création des arcs */

		// création de l'arc begin to start 
		Arc Arc_begin_start = pnFactory.createArc();
		Arc_begin_start.setSource(begin_p);
		Arc_begin_start.setCible(TransitionStart);
		Arc_begin_start.setType(ArcType.NORMAL);
		Arc_begin_start.setNb_jetons(1);
		petrinet.getPetrinetelement().add(Arc_begin_start);

		// création de l'arc start to started1
		Arc Arc_start_started1 = pnFactory.createArc();
		Arc_start_started1.setSource(TransitionStart);
		Arc_start_started1.setCible(started1_p);
		Arc_start_started1.setType(ArcType.NORMAL);
		Arc_start_started1.setNb_jetons(1);
		petrinet.getPetrinetelement().add(Arc_start_started1);

		// création de l'arc start to started2 
		Arc Arc_start_started2 = pnFactory.createArc();
		Arc_start_started2.setSource(TransitionStart);
		Arc_start_started2.setCible(started2_p);
		Arc_start_started2.setType(ArcType.NORMAL);
		Arc_start_started2.setNb_jetons(1);
		petrinet.getPetrinetelement().add(Arc_start_started2);


		// création de l'arc started1 to finish
		Arc Arc_started1_finish = pnFactory.createArc();
		Arc_started1_finish.setSource(started1_p);
		Arc_started1_finish.setCible(TransitionFinish);
		Arc_started1_finish.setType(ArcType.NORMAL);
		Arc_started1_finish.setNb_jetons(1);
		petrinet.getPetrinetelement().add(Arc_started1_finish);


		// création de l'arc finish to end
		Arc Arc_finish_end = pnFactory.createArc();
		Arc_finish_end.setSource(TransitionFinish);
		Arc_finish_end.setCible(end_p);
		Arc_finish_end.setType(ArcType.NORMAL);
		Arc_finish_end.setNb_jetons(1);
		petrinet.getPetrinetelement().add(Arc_finish_end);

	}

	// Convertir les WorkSequences
	private static void ConvertWS(WorkSequence ws) {
		// Création de l'arc
		
		Arc Arc = pnFactory.createArc();
		
		switch (ws.getLinkType()) {
		
		case START_TO_FINISH :
		Arc.setSource(started2.get(ws.getPredecessor().getName()));
		Arc.setCible(finish.get(ws.getSuccessor().getName()));
		break;		
		
		case START_TO_START :
			Arc.setSource(started2.get(ws.getPredecessor().getName()));
			Arc.setCible(start.get(ws.getSuccessor().getName()));
			break;
	
		case FINISH_TO_START :
			Arc.setSource(end.get(ws.getPredecessor().getName()));
			Arc.setCible(start.get(ws.getSuccessor().getName()));
			break;
		case FINISH_TO_FINISH :
			Arc.setSource(end.get(ws.getPredecessor().getName()));
			Arc.setCible(finish.get(ws.getSuccessor().getName()));
			break;
		
		default: break;
		
		}
		Arc.setType(ArcType.READARC);
		Arc.setNb_jetons(1);
		// Ajout de l'arc à petrinet
		petrinet.getPetrinetelement().add(Arc);
		
	}

	

	// Convertir Ressources
	private static void ConvertResource(Ressource rs) {
		// Création de la Place Ressource
		Place PlaceRessource = pnFactory.createPlace();

		// Nommer la ressource
		PlaceRessource.setName("Ressource_" + rs.getName());
		// Initialisation du nombre de jeton
		PlaceRessource.setNb_jetons(rs.getQte());
		
		// Ajout de la Place à petrinet
		petrinet.getPetrinetelement().add(PlaceRessource);
		
		// Liaison Ressource-WorkDefinition
		for (GestRessource d : rs.getGestRessource()) {
			// Ajout de l'arc entre la PlaceRessource et 
			// la transition start correspondante
			Arc arcDemande = pnFactory.createArc();
			arcDemande.setSource(PlaceRessource);
			arcDemande.setCible(start.get(d.getWorkdefinition().getName()));
			arcDemande.setNb_jetons(d.getQte_needed());
			petrinet.getPetrinetelement().add(arcDemande);
			
			// Ajout de l'arc entre la PlaceRessource et 
			// la transition finish correspondante
			Arc arcRetour = pnFactory.createArc();
			arcRetour.setSource(finish.get(d.getWorkdefinition().getName()));
			arcRetour.setCible(PlaceRessource);
			arcRetour.setNb_jetons(d.getQte_needed());
			petrinet.getPetrinetelement().add(arcRetour);
		}
	}


	public static void main(String[] args) {

		// Charger le package SimplePDL et petriNet afin de l'enregistrer dans le
		// registre
		// d'Eclipse.
		SimplepdlPackage packageInstancePDL = SimplepdlPackage.eINSTANCE;
		PetrinetPackage packageInstancePetri = PetrinetPackage.eINSTANCE;

		// Enregistrer l'extension ".xmi" comme devant Ãªtre ouverte Ã 
		// l'aide d'un objet "XMIResourceFactoryImpl"
		Resource.Factory.Registry reg = Resource.Factory.Registry.INSTANCE;
		Map<String, Object> m = reg.getExtensionToFactoryMap();
		m.put("xmi", new XMIResourceFactoryImpl());

		// CrÃ©er un objet resourceSetImpl qui contiendra une ressource EMF (le modÃ¨le)
		ResourceSet resSetPDL = new ResourceSetImpl();
		ResourceSet resSetPetri = new ResourceSetImpl();

		// DÃ©finir la ressource (le modÃ¨le)
		URI modelURIPetri = URI.createURI("models/PetriNet_developpement.xmi");
		Resource resourcePetri = resSetPetri.createResource(modelURIPetri);

		URI modelURIPDL = URI.createURI("models/developpement.xmi");
		Resource resourcePDL = resSetPDL.getResource(modelURIPDL, true);

		// On récupère le Process

		Process process = (Process) resourcePDL.getContents().get(0);
		
		// La fabrique pour fabriquer les Ã©lÃ©ments de PetriNet
		pnFactory = PetrinetFactory.eINSTANCE;

		// CrÃ©er un Ã©lÃ©ment PetriNet
		petrinet = pnFactory.createPetriNet();
		petrinet.setName(process.getName());

		// Ajout de la racine du petrinet dans le modèle
		resourcePetri.getContents().add(petrinet);

		// On parcourt tous les ProcessElements du modèle SimplePDL
		for (Object o : process.getProcessElements()) {
			if (o instanceof WorkDefinition) {
				ConvertWD((WorkDefinition) o);
			} else if (o instanceof WorkSequence) {
				ConvertWS((WorkSequence) o);
			} else if (o instanceof Ressource) {
				ConvertResource((Ressource) o);
			}
		}

		// Sauvgarder la ressource
		try {
			resourcePetri.save(Collections.EMPTY_MAP);
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

}
