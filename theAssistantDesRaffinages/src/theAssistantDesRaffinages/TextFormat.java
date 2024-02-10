package theAssistantDesRaffinages;
/**
 * TextFormat définit une énumération de liste de formats de texte possibles pour le formatage du texte.
 * Chaque format est associé à un code hexadécimal correspondant. 
 */
public enum TextFormat {
    RESET("\u001B[0m"),
    BOLD("\u001B[1m"),
    ITALIC("\u001B[3m"),
    UNDERLINE("\u001B[4m");

    private final String code;

    TextFormat(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }
}
