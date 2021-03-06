/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package e;

import e.exceptions.IllegalOrphanException;
import e.exceptions.NonexistentEntityException;
import e.exceptions.PreexistingEntityException;
import e.exceptions.RollbackFailureException;
import java.util.List;
import javax.annotation.Resource;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnit;
import javax.persistence.Query;
import javax.persistence.EntityNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import javax.transaction.UserTransaction;

/**
 *
 * @author vkraemer
 */
public class DiscountCodeJpaController {
    @Resource
    private UserTransaction utx = null;
    @PersistenceUnit(unitName = "JPAJSFPU")
    private EntityManagerFactory emf = null;

    public EntityManager getEntityManager() {
        return emf.createEntityManager();
    }

    public void create(DiscountCode discountCode) throws PreexistingEntityException, RollbackFailureException, Exception {
        if (discountCode.getCustomerCollection() == null) {
            discountCode.setCustomerCollection(new ArrayList<Customer>());
        }
        EntityManager em = null;
        try {
            utx.begin();
            em = getEntityManager();
            Collection<Customer> attachedCustomerCollection = new ArrayList<Customer>();
            for (Customer customerCollectionCustomerToAttach : discountCode.getCustomerCollection()) {
                customerCollectionCustomerToAttach = em.getReference(customerCollectionCustomerToAttach.getClass(), customerCollectionCustomerToAttach.getCustomerId());
                attachedCustomerCollection.add(customerCollectionCustomerToAttach);
            }
            discountCode.setCustomerCollection(attachedCustomerCollection);
            em.persist(discountCode);
            for (Customer customerCollectionCustomer : discountCode.getCustomerCollection()) {
                DiscountCode oldDiscountCodeOfCustomerCollectionCustomer = customerCollectionCustomer.getDiscountCode();
                customerCollectionCustomer.setDiscountCode(discountCode);
                customerCollectionCustomer = em.merge(customerCollectionCustomer);
                if (oldDiscountCodeOfCustomerCollectionCustomer != null) {
                    oldDiscountCodeOfCustomerCollectionCustomer.getCustomerCollection().remove(customerCollectionCustomer);
                    oldDiscountCodeOfCustomerCollectionCustomer = em.merge(oldDiscountCodeOfCustomerCollectionCustomer);
                }
            }
            utx.commit();
        } catch (Exception ex) {
            try {
                utx.rollback();
            } catch (Exception re) {
                throw new RollbackFailureException("An error occurred attempting to roll back the transaction.", re);
            }
            if (findDiscountCode(discountCode.getDiscountCode()) != null) {
                throw new PreexistingEntityException("DiscountCode " + discountCode + " already exists.", ex);
            }
            throw ex;
        } finally {
            if (em != null) {
                em.close();
            }
        }
    }

    public void edit(DiscountCode discountCode) throws IllegalOrphanException, NonexistentEntityException, RollbackFailureException, Exception {
        EntityManager em = null;
        try {
            utx.begin();
            em = getEntityManager();
            DiscountCode persistentDiscountCode = em.find(DiscountCode.class, discountCode.getDiscountCode());
            Collection<Customer> customerCollectionOld = persistentDiscountCode.getCustomerCollection();
            Collection<Customer> customerCollectionNew = discountCode.getCustomerCollection();
            List<String> illegalOrphanMessages = null;
            for (Customer customerCollectionOldCustomer : customerCollectionOld) {
                if (!customerCollectionNew.contains(customerCollectionOldCustomer)) {
                    if (illegalOrphanMessages == null) {
                        illegalOrphanMessages = new ArrayList<String>();
                    }
                    illegalOrphanMessages.add("You must retain Customer " + customerCollectionOldCustomer + " since its discountCode field is not nullable.");
                }
            }
            if (illegalOrphanMessages != null) {
                throw new IllegalOrphanException(illegalOrphanMessages);
            }
            Collection<Customer> attachedCustomerCollectionNew = new ArrayList<Customer>();
            for (Customer customerCollectionNewCustomerToAttach : customerCollectionNew) {
                customerCollectionNewCustomerToAttach = em.getReference(customerCollectionNewCustomerToAttach.getClass(), customerCollectionNewCustomerToAttach.getCustomerId());
                attachedCustomerCollectionNew.add(customerCollectionNewCustomerToAttach);
            }
            customerCollectionNew = attachedCustomerCollectionNew;
            discountCode.setCustomerCollection(customerCollectionNew);
            discountCode = em.merge(discountCode);
            for (Customer customerCollectionNewCustomer : customerCollectionNew) {
                if (!customerCollectionOld.contains(customerCollectionNewCustomer)) {
                    DiscountCode oldDiscountCodeOfCustomerCollectionNewCustomer = customerCollectionNewCustomer.getDiscountCode();
                    customerCollectionNewCustomer.setDiscountCode(discountCode);
                    customerCollectionNewCustomer = em.merge(customerCollectionNewCustomer);
                    if (oldDiscountCodeOfCustomerCollectionNewCustomer != null && !oldDiscountCodeOfCustomerCollectionNewCustomer.equals(discountCode)) {
                        oldDiscountCodeOfCustomerCollectionNewCustomer.getCustomerCollection().remove(customerCollectionNewCustomer);
                        oldDiscountCodeOfCustomerCollectionNewCustomer = em.merge(oldDiscountCodeOfCustomerCollectionNewCustomer);
                    }
                }
            }
            utx.commit();
        } catch (Exception ex) {
            try {
                utx.rollback();
            } catch (Exception re) {
                throw new RollbackFailureException("An error occurred attempting to roll back the transaction.", re);
            }
            String msg = ex.getLocalizedMessage();
            if (msg == null || msg.length() == 0) {
                Character id = discountCode.getDiscountCode();
                if (findDiscountCode(id) == null) {
                    throw new NonexistentEntityException("The discountCode with id " + id + " no longer exists.");
                }
            }
            throw ex;
        } finally {
            if (em != null) {
                em.close();
            }
        }
    }

    public void destroy(Character id) throws IllegalOrphanException, NonexistentEntityException, RollbackFailureException, Exception {
        EntityManager em = null;
        try {
            utx.begin();
            em = getEntityManager();
            DiscountCode discountCode;
            try {
                discountCode = em.getReference(DiscountCode.class, id);
                discountCode.getDiscountCode();
            } catch (EntityNotFoundException enfe) {
                throw new NonexistentEntityException("The discountCode with id " + id + " no longer exists.", enfe);
            }
            List<String> illegalOrphanMessages = null;
            Collection<Customer> customerCollectionOrphanCheck = discountCode.getCustomerCollection();
            for (Customer customerCollectionOrphanCheckCustomer : customerCollectionOrphanCheck) {
                if (illegalOrphanMessages == null) {
                    illegalOrphanMessages = new ArrayList<String>();
                }
                illegalOrphanMessages.add("This DiscountCode (" + discountCode + ") cannot be destroyed since the Customer " + customerCollectionOrphanCheckCustomer + " in its customerCollection field has a non-nullable discountCode field.");
            }
            if (illegalOrphanMessages != null) {
                throw new IllegalOrphanException(illegalOrphanMessages);
            }
            em.remove(discountCode);
            utx.commit();
        } catch (Exception ex) {
            try {
                utx.rollback();
            } catch (Exception re) {
                throw new RollbackFailureException("An error occurred attempting to roll back the transaction.", re);
            }
            throw ex;
        } finally {
            if (em != null) {
                em.close();
            }
        }
    }

    public List<DiscountCode> findDiscountCodeEntities() {
        return findDiscountCodeEntities(true, -1, -1);
    }

    public List<DiscountCode> findDiscountCodeEntities(int maxResults, int firstResult) {
        return findDiscountCodeEntities(false, maxResults, firstResult);
    }

    private List<DiscountCode> findDiscountCodeEntities(boolean all, int maxResults, int firstResult) {
        EntityManager em = getEntityManager();
        try {
            Query q = em.createQuery("select object(o) from DiscountCode as o");
            if (!all) {
                q.setMaxResults(maxResults);
                q.setFirstResult(firstResult);
            }
            return q.getResultList();
        } finally {
            em.close();
        }
    }

    public DiscountCode findDiscountCode(Character id) {
        EntityManager em = getEntityManager();
        try {
            return em.find(DiscountCode.class, id);
        } finally {
            em.close();
        }
    }

    public int getDiscountCodeCount() {
        EntityManager em = getEntityManager();
        try {
            Query q = em.createQuery("select count(o) from DiscountCode as o");
            return ((Long) q.getSingleResult()).intValue();
        } finally {
            em.close();
        }
    }

}
