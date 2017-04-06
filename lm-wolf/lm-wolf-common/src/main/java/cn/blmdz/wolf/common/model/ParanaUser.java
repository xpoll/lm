package cn.blmdz.wolf.common.model;

import java.util.List;

import cn.blmdz.home.common.model.BaseUser;

public class ParanaUser implements BaseUser {
   private static final long serialVersionUID = -2961193418926377287L;
   private Long id;
   private String name;
   private Integer type;
   private Long shopId;
   private List roles;

   public String getTypeName() {
      return null;
   }

   public Long getId() {
      return this.id;
   }

   public String getName() {
      return this.name;
   }

   public Integer getType() {
      return this.type;
   }

   public Long getShopId() {
      return this.shopId;
   }

   public List getRoles() {
      return this.roles;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setShopId(Long shopId) {
      this.shopId = shopId;
   }

   public void setRoles(List roles) {
      this.roles = roles;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ParanaUser)) {
         return false;
      } else {
         ParanaUser other = (ParanaUser)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$id = this.getId();
            Object other$id = other.getId();
            if(this$id == null) {
               if(other$id != null) {
                  return false;
               }
            } else if(!this$id.equals(other$id)) {
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

            Object this$type = this.getType();
            Object other$type = other.getType();
            if(this$type == null) {
               if(other$type != null) {
                  return false;
               }
            } else if(!this$type.equals(other$type)) {
               return false;
            }

            Object this$shopId = this.getShopId();
            Object other$shopId = other.getShopId();
            if(this$shopId == null) {
               if(other$shopId != null) {
                  return false;
               }
            } else if(!this$shopId.equals(other$shopId)) {
               return false;
            }

            Object this$roles = this.getRoles();
            Object other$roles = other.getRoles();
            if(this$roles == null) {
               if(other$roles != null) {
                  return false;
               }
            } else if(!this$roles.equals(other$roles)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof ParanaUser;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $id = this.getId();
      result = result * 59 + ($id == null?0:$id.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      Object $type = this.getType();
      result = result * 59 + ($type == null?0:$type.hashCode());
      Object $shopId = this.getShopId();
      result = result * 59 + ($shopId == null?0:$shopId.hashCode());
      Object $roles = this.getRoles();
      result = result * 59 + ($roles == null?0:$roles.hashCode());
      return result;
   }

   public String toString() {
      return "ParanaUser(id=" + this.getId() + ", name=" + this.getName() + ", type=" + this.getType() + ", shopId=" + this.getShopId() + ", roles=" + this.getRoles() + ")";
   }
}
