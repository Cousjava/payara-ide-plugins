
package jsfcontrollers;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import jpas.Book;

/**
 *
 * @author ludo
 */
public class BookConverter implements Converter {

    public Object getAsObject(FacesContext facesContext, UIComponent component, String string) {
        if (string == null || string.length() == 0) {
            return null;
        }
        Integer id = new Integer(string);
        BookController controller = (BookController) facesContext.getApplication().getELResolver().getValue(facesContext.getELContext(), null, "book");

        return controller.findBook(id);
    }

    public String getAsString(FacesContext facesContext, UIComponent component, Object object) {
        if (object == null) {
            return null;
        }
        if (object instanceof Book) {
            Book o = (Book) object;
            return o.getBookId() == null ? "" : o.getBookId().toString();
        } else {
            throw new IllegalArgumentException("object " + object + " is of type " + object.getClass().getName() + "; expected type: jpas.Book");
        }
    }

}
