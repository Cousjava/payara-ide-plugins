/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package e;

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
import javax.transaction.UserTransaction;

/**
 *
 * @author vkraemer
 */
public class CustomerJpaController {
    @Resource
    private UserTransaction utx = null;
    @PersistenceUnit(unitName = "JPAJSFPU")
    private EntityManagerFactory emf = null;

    public EntityManager getEntityManager() {
        return emf.createEntityManager();
    }

    public void create(Customer customer) throws PreexistingEntityException, RollbackFailureException, Exception {
        EntityManager em = null;
        try {
            utx.begin();
            em = getEntityManager();
            DiscountCode discountCode = customer.getDiscountCode();
            if (discountCode != null) {
                discountCode = em.getReference(discountCode.getClass(), discountCode.getDiscountCode());
                customer.setDiscountCode(discountCode);
            }
            em.persist(customer);
            if (discountCode != null) {
                discountCode.getCustomerCollection().add(customer);
                discountCode = em.merge(discountCode);
            }
            utx.commit();
        } catch (Exception ex) {
            try {
                utx.rollback();
            } catch (Exception re) {
                throw new RollbackFailureException("An error occurred attempting to roll back the transaction.", re);
            }
            if (findCustomer(customer.getCustomerId()) != null) {
                throw new PreexistingEntityException("Customer " + customer + " already exists.", ex);
            }
            throw ex;
        } finally {
            if (em != null) {
                em.close();
            }
        }
    }

    public void edit(Customer customer) throws NonexistentEntityException, RollbackFailureException, Exception {
        EntityManager em = null;
        try {
            utx.begin();
            em = getEntityManager();
            Customer persistentCustomer = em.find(Customer.class, customer.getCustomerId());
            DiscountCode discountCodeOld = persistentCustomer.getDiscountCode();
            DiscountCode discountCodeNew = customer.getDiscountCode();
            if (discountCodeNew != null) {
                discountCodeNew = em.getReference(discountCodeNew.getClass(), discountCodeNew.getDiscountCode());
                customer.setDiscountCode(discountCodeNew);
            }
            customer = em.merge(customer);
            if (discountCodeOld != null && !discountCodeOld.equals(discountCodeNew)) {
                discountCodeOld.getCustomerCollection().remove(customer);
                discountCodeOld = em.merge(discountCodeOld);
            }
            if (discountCodeNew != null && !discountCodeNew.equals(discountCodeOld)) {
                discountCodeNew.getCustomerCollection().add(customer);
                discountCodeNew = em.merge(discountCodeNew);
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
                Integer id = customer.getCustomerId();
                if (findCustomer(id) == null) {
                    throw new NonexistentEntityException("The customer with id " + id + " no longer exists.");
                }
            }
            throw ex;
        } finally {
            if (em != null) {
                em.close();
            }
        }
    }

    public void destroy(Integer id) throws NonexistentEntityException, RollbackFailureException, Exception {
        EntityManager em = null;
        try {
            utx.begin();
            em = getEntityManager();
            Customer customer;
            try {
                customer = em.getReference(Customer.class, id);
                customer.getCustomerId();
            } catch (EntityNotFoundException enfe) {
                throw new NonexistentEntityException("The customer with id " + id + " no longer exists.", enfe);
            }
            DiscountCode discountCode = customer.getDiscountCode();
            if (discountCode != null) {
                discountCode.getCustomerCollection().remove(customer);
                discountCode = em.merge(discountCode);
            }
            em.remove(customer);
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

    public List<Customer> findCustomerEntities() {
        return findCustomerEntities(true, -1, -1);
    }

    public List<Customer> findCustomerEntities(int maxResults, int firstResult) {
        return findCustomerEntities(false, maxResults, firstResult);
    }

    private List<Customer> findCustomerEntities(boolean all, int maxResults, int firstResult) {
        EntityManager em = getEntityManager();
        try {
            Query q = em.createQuery("select object(o) from Customer as o");
            if (!all) {
                q.setMaxResults(maxResults);
                q.setFirstResult(firstResult);
            }
            return q.getResultList();
        } finally {
            em.close();
        }
    }

    public Customer findCustomer(Integer id) {
        EntityManager em = getEntityManager();
        try {
            return em.find(Customer.class, id);
        } finally {
            em.close();
        }
    }

    public int getCustomerCount() {
        EntityManager em = getEntityManager();
        try {
            Query q = em.createQuery("select count(o) from Customer as o");
            return ((Long) q.getSingleResult()).intValue();
        } finally {
            em.close();
        }
    }

}
