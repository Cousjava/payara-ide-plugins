
package jsfcontrollers;

import java.util.List;
import java.util.Map;
import javax.annotation.Resource;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnit;
import javax.persistence.Query;
import javax.faces.application.FacesMessage;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import javax.faces.FacesException;
import java.util.HashMap;
import javax.faces.validator.ValidatorException;
import javax.transaction.UserTransaction;
import jpas.Book;

/**
 *
 * @author ludo
 */
public class BookController {
    private Book book = null;
    private List<Book> books = null;
    @Resource
    private UserTransaction utx = null;
    @PersistenceUnit(unitName = "WebAppWithEJBSPU")
    private EntityManagerFactory emf = null;

    public EntityManager getEntityManager() {
        return emf.createEntityManager();
    }
    public int batchSize = 5;
    private int firstItem = 0;
    private int itemCount = -1;

    public SelectItem[] getBooksAvailableSelectMany() {
        return getBooksAvailable(false);
    }

    public SelectItem[] getBooksAvailableSelectOne() {
        return getBooksAvailable(true);
    }

    private SelectItem[] getBooksAvailable(boolean one) {
        List<Book> allBooks = getBooks(true);
        int size = one ? allBooks.size() + 1 : allBooks.size();
        SelectItem[] items = new SelectItem[size];
        int i = 0;
        if (one) {
            items[0] = new SelectItem("", "---");
            i++;
        }
        for (Book x : allBooks) {
            items[i++] = new SelectItem(x, x.toString());
        }
        return items;
    }

    public Book getBook() {
        if (book == null) {
            book = getBookFromRequest();
        }
        if (book == null) {
            book = new Book();
        }
        return book;
    }

    public String listSetup() {
        reset(true);
        return "book_list";
    }

    public String createSetup() {
        reset(false);
        book = new Book();
        return "book_create";
    }

    public String create() {
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            em.persist(book);
            utx.commit();
            addSuccessMessage("Book was successfully created.");
        } catch (Exception ex) {
            try {
                if (findBook(book.getBookId()) != null) {
                    addErrorMessage("Book " + book + " already exists.");
                } else {
                    ensureAddErrorMessage(ex, "A persistence error occurred.");
                }
                utx.rollback();
            } catch (Exception e) {
                ensureAddErrorMessage(e, "An error occurred attempting to roll back the transaction.");
            }
            return null;
        } finally {
            em.close();
        }
        return listSetup();
    }

    public String detailSetup() {
        return scalarSetup("book_detail");
    }

    public String editSetup() {
        return scalarSetup("book_edit");
    }

    private String scalarSetup(String destination) {
        reset(false);
        book = getBookFromRequest();
        if (book == null) {
            String requestBookString = getRequestParameter("jsfcrud.currentBook");
            addErrorMessage("The book with id " + requestBookString + " no longer exists.");
            String relatedControllerOutcome = relatedControllerOutcome();
            if (relatedControllerOutcome != null) {
                return relatedControllerOutcome;
            }
            return listSetup();
        }
        return destination;
    }

    public String edit() {
        BookConverter converter = new BookConverter();
        String bookString = converter.getAsString(FacesContext.getCurrentInstance(), null, book);
        String currentBookString = getRequestParameter("jsfcrud.currentBook");
        if (bookString == null || bookString.length() == 0 || !bookString.equals(currentBookString)) {
            String outcome = editSetup();
            if ("book_edit".equals(outcome)) {
                addErrorMessage("Could not edit book. Try again.");
            }
            return outcome;
        }
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            book = em.merge(book);
            utx.commit();
            addSuccessMessage("Book was successfully updated.");
        } catch (Exception ex) {
            try {
                String msg = ex.getLocalizedMessage();
                if (msg != null && msg.length() > 0) {
                    addErrorMessage(msg);
                } else if (getBookFromRequest() == null) {
                    addErrorMessage("The book with id " + currentBookString + " no longer exists.");
                    utx.rollback();
                    return listSetup();
                } else {
                    addErrorMessage("A persistence error occurred.");
                }
                utx.rollback();
            } catch (Exception e) {
                ensureAddErrorMessage(e, "An error occurred attempting to roll back the transaction.");
            }
            return null;
        } finally {
            em.close();
        }
        return detailSetup();
    }

    public String destroy() {
        book = getBookFromRequest();
        if (book == null) {
            String currentBookString = getRequestParameter("jsfcrud.currentBook");
            addErrorMessage("The book with id " + currentBookString + " no longer exists.");
            String relatedControllerOutcome = relatedControllerOutcome();
            if (relatedControllerOutcome != null) {
                return relatedControllerOutcome;
            }
            return listSetup();
        }
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            book = em.getReference(book.getClass(), book.getBookId());
            em.remove(book);
            utx.commit();
            addSuccessMessage("Book was successfully deleted.");
        } catch (Exception ex) {
            try {
                ensureAddErrorMessage(ex, "A persistence error occurred.");
                utx.rollback();
            } catch (Exception e) {
                ensureAddErrorMessage(e, "An error occurred attempting to roll back the transaction.");
            }
            return null;
        } finally {
            em.close();
        }
        String relatedControllerOutcome = relatedControllerOutcome();
        if (relatedControllerOutcome != null) {
            return relatedControllerOutcome;
        }
        return listSetup();
    }

    private Book getBookFromRequest() {
        String theId = getRequestParameter("jsfcrud.currentBook");
        return (Book) new BookConverter().getAsObject(FacesContext.getCurrentInstance(), null, theId);
    }

    private String getRequestParameter(String key) {
        return FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get(key);
    }

    public List<Book> getBooks() {
        if (books == null) {
            books = getBooks(false);
        }
        return books;
    }

    public List<Book> getBooks(boolean all) {
        EntityManager em = getEntityManager();
        try {
            Query q = em.createQuery("select object(o) from Book as o");
            if (!all) {
                q.setMaxResults(batchSize);
                q.setFirstResult(getFirstItem());
            }
            return q.getResultList();
        } finally {
            em.close();
        }
    }

    private void ensureAddErrorMessage(Exception ex, String defaultMsg) {
        String msg = ex.getLocalizedMessage();
        if (msg != null && msg.length() > 0) {
            addErrorMessage(msg);
        } else {
            addErrorMessage(defaultMsg);
        }
    }

    public static void addErrorMessage(String msg) {
        FacesMessage facesMsg = new FacesMessage(FacesMessage.SEVERITY_ERROR, msg, msg);
        FacesContext.getCurrentInstance().addMessage(null, facesMsg);
    }

    public static void addSuccessMessage(String msg) {
        FacesMessage facesMsg = new FacesMessage(FacesMessage.SEVERITY_INFO, msg, msg);
        FacesContext.getCurrentInstance().addMessage("successInfo", facesMsg);
    }

    public Book findBook(Integer id) {
        EntityManager em = getEntityManager();
        try {
            Book o = (Book) em.find(Book.class, id);
            return o;
        } finally {
            em.close();
        }
    }

    public int getItemCount() {
        if (itemCount == -1) {
            EntityManager em = getEntityManager();
            try {
                itemCount = ((Long) em.createQuery("select count(o) from Book as o").getSingleResult()).intValue();
            } finally {
                em.close();
            }
        }
        return itemCount;
    }

    public int getFirstItem() {
        getItemCount();
        if (firstItem >= itemCount) {
            if (itemCount == 0) {
                firstItem = 0;
            } else {
                int zeroBasedItemCount = itemCount - 1;
                double pageDouble = zeroBasedItemCount / batchSize;
                int page = (int) Math.floor(pageDouble);
                firstItem = page * batchSize;
            }
        }
        return firstItem;
    }

    public int getLastItem() {
        getFirstItem();
        return firstItem + batchSize > itemCount ? itemCount : firstItem + batchSize;
    }

    public int getBatchSize() {
        return batchSize;
    }

    public String next() {
        reset(false);
        getFirstItem();
        if (firstItem + batchSize < itemCount) {
            firstItem += batchSize;
        }
        return "book_list";
    }

    public String prev() {
        reset(false);
        getFirstItem();
        firstItem -= batchSize;
        if (firstItem < 0) {
            firstItem = 0;
        }
        return "book_list";
    }

    private String relatedControllerOutcome() {
        String relatedControllerString = getRequestParameter("jsfcrud.relatedController");
        String relatedControllerTypeString = getRequestParameter("jsfcrud.relatedControllerType");
        if (relatedControllerString != null && relatedControllerTypeString != null) {
            FacesContext context = FacesContext.getCurrentInstance();
            Object relatedController = context.getApplication().getELResolver().getValue(context.getELContext(), null, relatedControllerString);
            try {
                Class<?> relatedControllerType = Class.forName(relatedControllerTypeString);
                Method detailSetupMethod = relatedControllerType.getMethod("detailSetup");
                return (String) detailSetupMethod.invoke(relatedController);
            } catch (ClassNotFoundException e) {
                throw new FacesException(e);
            } catch (NoSuchMethodException e) {
                throw new FacesException(e);
            } catch (IllegalAccessException e) {
                throw new FacesException(e);
            } catch (InvocationTargetException e) {
                throw new FacesException(e);
            }
        }
        return null;
    }

    private void reset(boolean resetFirstItem) {
        book = null;
        books = null;
        itemCount = -1;
        if (resetFirstItem) {
            firstItem = 0;
        }
    }
    private Map<Object, String> asString = null;

    public Map<Object, String> getAsString() {
        if (asString == null) {
            asString = new HashMap<Object, String>() {

                @Override
                public String get(Object key) {
                    if (key instanceof Object[]) {
                        Object[] keyAsArray = (Object[]) key;
                        if (keyAsArray.length == 0) {
                            return "(No Items)";
                        }
                        StringBuffer sb = new StringBuffer();
                        for (int i = 0; i < keyAsArray.length; i++) {
                            if (i > 0) {
                                sb.append("<br />");
                            }
                            sb.append(keyAsArray[i]);
                        }
                        return sb.toString();
                    }
                    return new BookConverter().getAsString(FacesContext.getCurrentInstance(), null, (Book) key);
                }
            };
        }
        return asString;
    }

    public void validateCreate(FacesContext facesContext, UIComponent component, Object value) {
        BookConverter converter = new BookConverter();
        String newBookString = converter.getAsString(FacesContext.getCurrentInstance(), null, new Book());
        String bookString = converter.getAsString(FacesContext.getCurrentInstance(), null, book);
        if (!newBookString.equals(bookString)) {
            createSetup();
        }
    }

}
