
package jsfcontrollers;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import jpas.Author;

/**
 *
 * @author ludo
 */
public class AuthorConverter implements Converter {

    public Object getAsObject(FacesContext facesContext, UIComponent component, String string) {
        if (string == null || string.length() == 0) {
            return null;
        }
        Integer id = new Integer(string);
        AuthorController controller = (AuthorController) facesContext.getApplication().getELResolver().getValue(facesContext.getELContext(), null, "author");

        return controller.findAuthor(id);
    }

    public String getAsString(FacesContext facesContext, UIComponent component, Object object) {
        if (object == null) {
            return null;
        }
        if (object instanceof Author) {
            Author o = (Author) object;
            return o.getAuthorId() == null ? "" : o.getAuthorId().toString();
        } else {
            throw new IllegalArgumentException("object " + object + " is of type " + object.getClass().getName() + "; expected type: jpas.Author");
        }
    }

}
