package theAssistantDesRaffinages;

import static org.junit.Assert.assertEquals;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

/** Programme de test de la classe ActionComplexe.
 */
public class TestActionComplexe {
	
	public static final double EPSILON = 1e-6;
	// précision pour la comparaison entre réels.
	private ActionComplexe r1_1;
	private ActionComplexe r1_2;
	private ActionComplexe r2_1;
	private ActionComplexe r2_2;

	
	@Before
	public void setUp() {
		r1_1 = new ActionComplexe("R1: 1", 1);
		r1_2 = new ActionComplexe("R1: 2", 1);
		r2_1 = new ActionComplexe("R2: 1", 2);
		r2_2 = new ActionComplexe("R2: 2", 2);
	}
	
	@Test
	public void testInitialisation() {
		assertNotNull(r1_1);
		assertEquals(r1_1.getNiveau(),1, EPSILON);
		assertTrue(r1_1.estVide());
		assertTrue(r1_1.getCouleur() == TextColor.BLACK);
	}
	
	@Test
	public void testNiveau() {
		assertEquals(r1_1.getNiveau(),1, EPSILON);
		r1_1.setNiveau(3);
		assertEquals(r1_1.getNiveau(), 3, EPSILON);
		r1_1.incrementerNiveau(1);
		assertEquals(r1_1.getNiveau(), 4, EPSILON);
	}
	
	@Test
	public void testSousFils() {
		List<Element> nouveausousFils = new ArrayList<>();
		nouveausousFils.add(r2_1);
		nouveausousFils.add(r2_2);
		assertTrue(r1_1.estVide());
		r1_1.addElement(r2_1);
		r1_1.addElement(r2_2);
		assertEquals(r1_1.getElements().get(0), r2_1);
		assertTrue(r1_1.getElements().contains(r2_1));
		assertTrue(r1_1.getElements().contains(r2_2));
		r1_1.removeElement(r2_1);
		r1_1.removeElement(r2_2);
		assertTrue(r1_1.estVide());	
	}
	
	@Test
	public void testCouleur() { 
		assertEquals(r1_1.getCouleur(), TextColor.BLACK);
		r1_1.setCouleur(TextColor.GREEN);
		assertEquals(r1_1.getCouleur(), TextColor.GREEN);
	}
	
	
	
}
