package cn.blmdz.wolf.parana.category.model;

import java.io.Serializable;
import java.util.Date;

public class CategoryBinding implements Serializable {
   private static final long serialVersionUID = 6760480438124681550L;
   private Long id;
   private Long frontCategoryId;
   private Long backCategoryId;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public Long getFrontCategoryId() {
      return this.frontCategoryId;
   }

   public void setFrontCategoryId(Long frontCategoryId) {
      this.frontCategoryId = frontCategoryId;
   }

   public Long getBackCategoryId() {
      return this.backCategoryId;
   }

   public void setBackCategoryId(Long backCategoryId) {
      this.backCategoryId = backCategoryId;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }
}
