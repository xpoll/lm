package cn.blmdz.wolf.parana.category.model;

import java.io.Serializable;
import java.util.Date;

public class ShopCategory implements Serializable, Comparable<ShopCategory> {
   private static final long serialVersionUID = 6476140144465999325L;
   public static final long ID_ROOT = 0L;
   public static final long ID_UNKNOWN = -1L;
   private Long id;
   private Long shopId;
   private Long pid;
   private String name;
   private Integer level;
   private Boolean hasChildren;
   private Boolean hasItem;
   private Integer index;
   private Boolean disclosed;
   private Date createdAt;
   private Date updatedAt;

   public int compareTo(ShopCategory o) {
      if(o == null) {
         return 1;
      } else {
         int pr = this.pid.compareTo(o.pid);
         return pr != 0?pr:(this.index != null?(o.index == null?-1:this.index.compareTo(o.index)):(o.index == null?this.id.compareTo(o.id):1));
      }
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ShopCategory)) {
         return false;
      } else {
         ShopCategory other = (ShopCategory)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$shopId = this.getShopId();
            Object other$shopId = other.getShopId();
            if(this$shopId == null) {
               if(other$shopId != null) {
                  return false;
               }
            } else if(!this$shopId.equals(other$shopId)) {
               return false;
            }

            Object this$pid = this.getPid();
            Object other$pid = other.getPid();
            if(this$pid == null) {
               if(other$pid != null) {
                  return false;
               }
            } else if(!this$pid.equals(other$pid)) {
               return false;
            }

            Object this$name = this.getName();
            Object other$name = other.getName();
            if(this$name == null) {
               if(other$name != null) {
                  return false;
               }
            } else if(!this$name.equals(other$name)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ShopCategory;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $shopId = this.getShopId();
      result = result * 59 + ($shopId == null?0:$shopId.hashCode());
      Object $pid = this.getPid();
      result = result * 59 + ($pid == null?0:$pid.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      return result;
   }

   public String toString() {
      return "ShopCategory(id=" + this.getId() + ", shopId=" + this.getShopId() + ", pid=" + this.getPid() + ", name=" + this.getName() + ", level=" + this.getLevel() + ", hasChildren=" + this.getHasChildren() + ", hasItem=" + this.getHasItem() + ", index=" + this.getIndex() + ", disclosed=" + this.getDisclosed() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public Long getId() {
      return this.id;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public Long getPid() {
      return this.pid;
   }

   public void setPid(Long pid) {
      this.pid = pid;
   }

   public String getName() {
      return this.name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public Integer getLevel() {
      return this.level;
   }

   public void setLevel(Integer level) {
      this.level = level;
   }

   public Boolean getHasChildren() {
      return this.hasChildren;
   }

   public void setHasChildren(Boolean hasChildren) {
      this.hasChildren = hasChildren;
   }

   public Boolean getHasItem() {
      return this.hasItem;
   }

   public void setHasItem(Boolean hasItem) {
      this.hasItem = hasItem;
   }

   public Integer getIndex() {
      return this.index;
   }

   public void setIndex(Integer index) {
      this.index = index;
   }

   public Boolean getDisclosed() {
      return this.disclosed;
   }

   public void setDisclosed(Boolean disclosed) {
      this.disclosed = disclosed;
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
