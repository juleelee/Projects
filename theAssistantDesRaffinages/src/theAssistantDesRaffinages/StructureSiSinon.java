package theAssistantDesRaffinages;

import java.util.ArrayList;
import java.util.List;

public class StructureSiSinon extends StructureDeControle {

	List<Element> SinonCorps;
	
    public StructureSiSinon(String condition, String nom) {
        super(condition, nom);
        SinonCorps = new ArrayList<>();
    }
	public StructureSiSinon(int id,String condition,String nom) {
		super(id,condition,nom);
	}

    @Override
    public String toStringAbstrait() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(TextFormat.BOLD.getCode()); // Appliquer le format en gras
        stringBuilder.append(TextColor.YELLOW.getCode()); // Appliquer la couleur jaune
        stringBuilder.append("Si (").append(getCondition()).append(") Alors :");
        stringBuilder.append(TextFormat.RESET.getCode()); // Réinitialiser le format

        // Affichage des éléments dans la partie "Alors"
        for (Element struct : getCorps()) {
            stringBuilder.append("\n\t").append(struct.toString());
        }

        stringBuilder.append(TextFormat.BOLD.getCode()); // Appliquer le format en gras
        stringBuilder.append(TextColor.BLUE.getCode()); // Appliquer la couleur bleue
        stringBuilder.append("\nSinon :");
        stringBuilder.append(TextFormat.RESET.getCode()); // Réinitialiser le format

        // Affichage des éléments dans la partie "Sinon"
        for (Element struct : SinonCorps) {
            stringBuilder.append("\n\t").append(struct.toString());
        }

        stringBuilder.append(TextFormat.BOLD.getCode()); // Appliquer le format en gras
        stringBuilder.append(TextColor.YELLOW.getCode()); // Appliquer la couleur jaune
        stringBuilder.append("\nFinSi\n");
        stringBuilder.append(TextFormat.RESET.getCode()); // Réinitialiser le format

        return stringBuilder.toString();
    }
}
