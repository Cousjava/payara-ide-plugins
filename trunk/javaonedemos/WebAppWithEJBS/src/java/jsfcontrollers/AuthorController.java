
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
import jpas.Author;

/**
 *
 * @author ludo
 */
public class AuthorController {
    private Author author = null;
    private List<Author> authors = null;
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

    public SelectItem[] getAuthorsAvailableSelectMany() {
        return getAuthorsAvailable(false);
    }

    public SelectItem[] getAuthorsAvailableSelectOne() {
        return getAuthorsAvailable(true);
    }

    private SelectItem[] getAuthorsAvailable(boolean one) {
        List<Author> allAuthors = getAuthors(true);
        int size = one ? allAuthors.size() + 1 : allAuthors.size();
        SelectItem[] items = new SelectItem[size];
        int i = 0;
        if (one) {
            items[0] = new SelectItem("", "---");
            i++;
        }
        for (Author x : allAuthors) {
            items[i++] = new SelectItem(x, x.toString());
        }
        return items;
    }

    public Author getAuthor() {
        if (author == null) {
            author = getAuthorFromRequest();
        }
        if (author == null) {
            author = new Author();
        }
        return author;
    }

    public String listSetup() {
        reset(true);
        return "author_list";
    }

    public String createSetup() {
        reset(false);
        author = new Author();
        return "author_create";
    }

    public String create() {
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            em.persist(author);
            utx.commit();
            addSuccessMessage("Author was successfully created.");
        } catch (Exception ex) {
            try {
                if (findAuthor(author.getAuthorId()) != null) {
                    addErrorMessage("Author " + author + " already exists.");
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
        return scalarSetup("author_detail");
    }

    public String editSetup() {
        return scalarSetup("author_edit");
    }

    private String scalarSetup(String destination) {
        reset(false);
        author = getAuthorFromRequest();
        if (author == null) {
            String requestAuthorString = getRequestParameter("jsfcrud.currentAuthor");
            addErrorMessage("The author with id " + requestAuthorString + " no longer exists.");
            String relatedControllerOutcome = relatedControllerOutcome();
            if (relatedControllerOutcome != null) {
                return relatedControllerOutcome;
            }
            return listSetup();
        }
        return destination;
    }

    public String edit() {
        AuthorConverter converter = new AuthorConverter();
        String authorString = converter.getAsString(FacesContext.getCurrentInstance(), null, author);
        String currentAuthorString = getRequestParameter("jsfcrud.currentAuthor");
        if (authorString == null || authorString.length() == 0 || !authorString.equals(currentAuthorString)) {
            String outcome = editSetup();
            if ("author_edit".equals(outcome)) {
                addErrorMessage("Could not edit author. Try again.");
            }
            return outcome;
        }
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            author = em.merge(author);
            utx.commit();
            addSuccessMessage("Author was successfully updated.");
        } catch (Exception ex) {
            try {
                String msg = ex.getLocalizedMessage();
                if (msg != null && msg.length() > 0) {
                    addErrorMessage(msg);
                } else if (getAuthorFromRequest() == null) {
                    addErrorMessage("The author with id " + currentAuthorString + " no longer exists.");
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
        author = getAuthorFromRequest();
        if (author == null) {
            String currentAuthorString = getRequestParameter("jsfcrud.currentAuthor");
            addErrorMessage("The author with id " + currentAuthorString + " no longer exists.");
            String relatedControllerOutcome = relatedControllerOutcome();
            if (relatedControllerOutcome != null) {
                return relatedControllerOutcome;
            }
            return listSetup();
        }
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            author = em.getReference(author.getClass(), author.getAuthorId());
            em.remove(author);
            utx.commit();
            addSuccessMessage("Author was successfully deleted.");
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

    private Author getAuthorFromRequest() {
        String theId = getRequestParameter("jsfcrud.currentAuthor");
        return (Author) new AuthorConverter().getAsObject(FacesContext.getCurrentInstance(), null, theId);
    }

    private String getRequestParameter(String key) {
        return FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get(key);
    }

    public List<Author> getAuthors() {
        if (authors == null) {
            authors = getAuthors(false);
        }
        return authors;
    }

    public List<Author> getAuthors(boolean all) {
        EntityManager em = getEntityManager();
        try {
            Query q = em.createQuery("select object(o) from Author as o");
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

    public Author findAuthor(Integer id) {
        EntityManager em = getEntityManager();
        try {
            Author o = (Author) em.find(Author.class, id);
            return o;
        } finally {
            em.close();
        }
    }

    public int getItemCount() {
        if (itemCount == -1) {
            EntityManager em = getEntityManager();
            try {
                itemCount = ((Long) em.createQuery("select count(o) from Author as o").getSingleResult()).intValue();
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
        return "author_list";
    }

    public String prev() {
        reset(false);
        getFirstItem();
        firstItem -= batchSize;
        if (firstItem < 0) {
            firstItem = 0;
        }
        return "author_list";
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
        author = null;
        authors = null;
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
                    return new AuthorConverter().getAsString(FacesContext.getCurrentInstance(), null, (Author) key);
                }
            };
        }
        return asString;
    }

    public void validateCreate(FacesContext facesContext, UIComponent component, Object value) {
        AuthorConverter converter = new AuthorConverter();
        String newAuthorString = converter.getAsString(FacesContext.getCurrentInstance(), null, new Author());
        String authorString = converter.getAsString(FacesContext.getCurrentInstance(), null, author);
        if (!newAuthorString.equals(authorString)) {
            createSetup();
        }
    }

}
