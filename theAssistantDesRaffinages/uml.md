```
@startuml
package "Assistant de raffinages" #DDDDDD {

    interface Element{
       {method} toString() : String
    }

    interface Action {
        + {method} + addFormat(TextFormat format)
	+ {method} + removeFormat(TextFormat format)
    
	+ {method} + getCouleur() : TextColor 
	+ {method} + setCouleur(TextColor nouvelleCouleur)
    
	+ {method} + getNiveau() : int 
    
	+ {method} + getTexteFormate() : String 
    
	+ {method} + print();

        + {method} + getElementId() :  int	
	+ {method} + setElementId(int elt) 

    }

    abstract StructureDeControle {
        + {field} + StructureDeControle(String condition,String nom)
        + {field} + StructureDeControle(int id, String condition,String nom)

	+ {field} + StructureDeControle(String nom,String var,String debut,String fin)	
	+ {field} + StructureDeControle(String nom, int id, String var,String debut,String fin) {
		
	+ {method} + add(Element struct) 
	
	+ {method} + delete(Element struct)
	
	+ {method} + setCondition(String newCondition) 
	
	+ {method} + getNom() : String 
	
	+ {method} + getCondition() : String 
	
	+ {method} +  getCorps() : List<Element>

	+ {method} + getVar() : String 
	+ {method} + setVar(String var) 

	+ {method} + getDebut() : String 
	+ {method} + setDebut(String debut) 

	+ {method} + getFin() : String 
	+ {method} + setFin(String fin)

        + {method} + getElementId() :  int	
	+ {method} + setElementId(int elt)  

        + {method} + abstract toStringAbstrait() :  String

    }

    class ActionElementaire {
        + {field} + ActionElementaire (VueEditionRaffinages textArea) 
        + {field} + ActionElementaire ()      
    }

    class ActionComplexe {
        + {field} + ActionComplexe (VueEditionRaffinages textArea) 
        + {field} + ActionComplexe (String titre, int niveau) 
        + {field} + ActionComplexe (String titre, int niveau,int id)
    }

    class StructureSi {
        + {field} + StructureSi(String condition,String nom) 
        + {field} + StructureSi(int id,String condition,String nom) 

        + {method} + toStringAbstrait() : String
        + {method} + toString() : String

    }


    class StructurePour {
        + {field} + StructurePour(String nom, String var, String debut, String fin)
        + {field} + StructurePour(String nom, int id, String var, String debut, String fin)

        + {method} + toStringAbstrait() : String
        + {method} + toString() : String

    }

    class StructureTantQue {
        + {field} + StructureTantque(String condition,String nom) 
        + {field} + StructureTantque(int id,String condition,String nom)
 
	+ {method} + toStringAbstrait() : String
        + {method} + toString() : String

    }


    note right of StructureSi
        Généralisation pour toutes 
        les structures de contrôle 
        connues (SISinon, Répéter, etc.)
    end note



    StructureDeControle o-- Element 
    StructureTantQue-->StructureDeControle : extends
    StructureSi--> StructureDeControle : extends

    ActionComplexe o-- Element
    StructureDeControle --> Element
    ActionElementaire--Action : implements
    ActionComplexe --Action : implements


}
@enduml
```
