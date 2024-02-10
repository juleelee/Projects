package theAssistantDesRaffinages;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;


import org.junit.Before;
import org.junit.Test;

/** Programme de test de la classe ActionComplexe.
 */
public class TestActionElementaire {
	
	public static final double EPSILON = 1e-6;
	// précision pour la comparaison entre réels.
	private ActionElementaire tester;

	
	@Before
	public void setUp() {
		tester = new ActionElementaire();
	}
	
	@Test
	public void testInitialisation() {
		assertNotNull(tester);
		tester.setNiveau(1);
		assertEquals(tester.getNiveau(),1, EPSILON);
		assertTrue(tester.getCouleur() == TextColor.BLACK);
	}
	
	@Test
	public void testNiveau() {
		assertEquals(tester.getNiveau(),0, EPSILON);
		tester.setNiveau(3);
		assertEquals(tester.getNiveau(), 3, EPSILON);
		tester.incrementerNiveau(1);
		assertEquals(tester.getNiveau(), 4, EPSILON);
	}
	
	
	@Test
	public void testCouleur() { 
		assertEquals(tester.getCouleur(), TextColor.BLACK);
		tester.setCouleur(TextColor.GREEN);
		assertEquals(tester.getCouleur(), TextColor.GREEN);
	}
	
	@Test
	public void testTitre() { 
		tester.setTitre("Tester action élementaire");
		assertEquals(tester.toString(), tester.getTexteFormate() + "\n ");
		
	}
		
}
