/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jpas;

import java.io.Serializable;
import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

/**
 *
 * @author ludo
 */
@Entity
@Table(name = "BOOK")
@NamedQueries({@NamedQuery(name = "Book.findByBookId", query = "SELECT b FROM Book b WHERE b.bookId = :bookId"), @NamedQuery(name = "Book.findByIsbn", query = "SELECT b FROM Book b WHERE b.isbn = :isbn"), @NamedQuery(name = "Book.findByAuthorid", query = "SELECT b FROM Book b WHERE b.authorid = :authorid"), @NamedQuery(name = "Book.findByPublisheddate", query = "SELECT b FROM Book b WHERE b.publisheddate = :publisheddate"), @NamedQuery(name = "Book.findByTitle", query = "SELECT b FROM Book b WHERE b.title = :title")})
public class Book implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "BOOK_ID", nullable = false)
    private Integer bookId;
    @Column(name = "ISBN")
    private String isbn;
    @Column(name = "AUTHORID")
    private Integer authorid;
    @Column(name = "PUBLISHEDDATE")
    @Temporal(TemporalType.DATE)
    private Date publisheddate;
    @Column(name = "TITLE")
    private String title;

    public Book() {
    }

    public Book(Integer bookId) {
        this.bookId = bookId;
    }

    public Integer getBookId() {
        return bookId;
    }

    public void setBookId(Integer bookId) {
        this.bookId = bookId;
    }

    public String getIsbn() {
        return isbn;
    }

    public void setIsbn(String isbn) {
        this.isbn = isbn;
    }

    public Integer getAuthorid() {
        return authorid;
    }

    public void setAuthorid(Integer authorid) {
        this.authorid = authorid;
    }

    public Date getPublisheddate() {
        return publisheddate;
    }

    public void setPublisheddate(Date publisheddate) {
        this.publisheddate = publisheddate;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (bookId != null ? bookId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Book)) {
            return false;
        }
        Book other = (Book) object;
        if ((this.bookId == null && other.bookId != null) || (this.bookId != null && !this.bookId.equals(other.bookId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "jpas.Book[bookId=" + bookId + "]";
    }

}
