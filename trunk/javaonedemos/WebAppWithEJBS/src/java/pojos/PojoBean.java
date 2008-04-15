package pojos;

import javax.ejb.Stateless;

@Stateless
public class PojoBean implements PojoInterface {

    public String businessMethod() {
        return "I am an EJB BusinessMethod of an EJB inside a Web App!!!";
    }
     
}
