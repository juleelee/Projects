package theAssistantDesRaffinages;
/**
 * TextColor définit une énumération qui définit une liste de couleurs de texte 
 * possibles pour le formatage du texte.
 *  Chaque couleur est associée à un code hexadécimal correspondant. 
 */
public enum TextColor {
	BLACK("#000000"),
    RED("#FF0000"),
    GREEN("#008000"),
    YELLOW("#FFFF00"),
    BLUE("#0000FF"),
    PURPLE("#800080"),
    WHITE("#FFFFFF");

    private final String code;

    TextColor(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }
}



