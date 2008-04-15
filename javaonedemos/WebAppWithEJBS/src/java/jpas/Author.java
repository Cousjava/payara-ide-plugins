/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jpas;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 *
 * @author ludo
 */
@Entity
@Table(name = "AUTHOR")
@NamedQueries({@NamedQuery(name = "Author.findByAuthorId", query = "SELECT a FROM Author a WHERE a.authorId = :authorId"), @NamedQuery(name = "Author.findByOrganisation", query = "SELECT a FROM Author a WHERE a.organisation = :organisation"), @NamedQuery(name = "Author.findByName", query = "SELECT a FROM Author a WHERE a.name = :name")})
public class Author implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "AUTHOR_ID", nullable = false)
    private Integer authorId;
    @Column(name = "ORGANISATION")
    private String organisation;
    @Column(name = "NAME")
    private String name;

    public Author() {
    }

    public Author(Integer authorId) {
        this.authorId = authorId;
    }

    public Integer getAuthorId() {
        return authorId;
    }

    public void setAuthorId(Integer authorId) {
        this.authorId = authorId;
    }

    public String getOrganisation() {
        return organisation;
    }

    public void setOrganisation(String organisation) {
        this.organisation = organisation;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (authorId != null ? authorId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Author)) {
            return false;
        }
        Author other = (Author) object;
        if ((this.authorId == null && other.authorId != null) || (this.authorId != null && !this.authorId.equals(other.authorId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "jpas.Author[authorId=" + authorId + "]";
    }

}
