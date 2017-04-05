package io.terminus.parana.order.model;

import java.io.Serializable;
import java.util.Date;

public class OrderActionInstance implements Serializable {
   private static final long serialVersionUID = -1537298526570093156L;
   private Long id;
   private Long nodeInstanceId;
   private String action;
   private Boolean display;
   private Integer type;
   private String belongUserTypes;
   private String name;
   private String desc;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getNodeInstanceId() {
      return this.nodeInstanceId;
   }

   public String getAction() {
      return this.action;
   }

   public Boolean getDisplay() {
      return this.display;
   }

   public Integer getType() {
      return this.type;
   }

   public String getBelongUserTypes() {
      return this.belongUserTypes;
   }

   public String getName() {
      return this.name;
   }

   public String getDesc() {
      return this.desc;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setNodeInstanceId(Long nodeInstanceId) {
      this.nodeInstanceId = nodeInstanceId;
   }

   public void setAction(String action) {
      this.action = action;
   }

   public void setDisplay(Boolean display) {
      this.display = display;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setBelongUserTypes(String belongUserTypes) {
      this.belongUserTypes = belongUserTypes;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setDesc(String desc) {
      this.desc = desc;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof OrderActionInstance)) {
         return false;
      } else {
         OrderActionInstance other = (OrderActionInstance)o;
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

            Object this$nodeInstanceId = this.getNodeInstanceId();
            Object other$nodeInstanceId = other.getNodeInstanceId();
            if(this$nodeInstanceId == null) {
               if(other$nodeInstanceId != null) {
                  return false;
               }
            } else if(!this$nodeInstanceId.equals(other$nodeInstanceId)) {
               return false;
            }

            Object this$action = this.getAction();
            Object other$action = other.getAction();
            if(this$action == null) {
               if(other$action != null) {
                  return false;
               }
            } else if(!this$action.equals(other$action)) {
               return false;
            }

            Object this$display = this.getDisplay();
            Object other$display = other.getDisplay();
            if(this$display == null) {
               if(other$display != null) {
                  return false;
               }
            } else if(!this$display.equals(other$display)) {
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

            Object this$belongUserTypes = this.getBelongUserTypes();
            Object other$belongUserTypes = other.getBelongUserTypes();
            if(this$belongUserTypes == null) {
               if(other$belongUserTypes != null) {
                  return false;
               }
            } else if(!this$belongUserTypes.equals(other$belongUserTypes)) {
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

            Object this$desc = this.getDesc();
            Object other$desc = other.getDesc();
            if(this$desc == null) {
               if(other$desc != null) {
                  return false;
               }
            } else if(!this$desc.equals(other$desc)) {
               return false;
            }

            Object this$createdAt = this.getCreatedAt();
            Object other$createdAt = other.getCreatedAt();
            if(this$createdAt == null) {
               if(other$createdAt != null) {
                  return false;
               }
            } else if(!this$createdAt.equals(other$createdAt)) {
               return false;
            }

            Object this$updatedAt = this.getUpdatedAt();
            Object other$updatedAt = other.getUpdatedAt();
            if(this$updatedAt == null) {
               if(other$updatedAt != null) {
                  return false;
               }
            } else if(!this$updatedAt.equals(other$updatedAt)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof OrderActionInstance;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $id = this.getId();
      result = result * 59 + ($id == null?0:$id.hashCode());
      Object $nodeInstanceId = this.getNodeInstanceId();
      result = result * 59 + ($nodeInstanceId == null?0:$nodeInstanceId.hashCode());
      Object $action = this.getAction();
      result = result * 59 + ($action == null?0:$action.hashCode());
      Object $display = this.getDisplay();
      result = result * 59 + ($display == null?0:$display.hashCode());
      Object $type = this.getType();
      result = result * 59 + ($type == null?0:$type.hashCode());
      Object $belongUserTypes = this.getBelongUserTypes();
      result = result * 59 + ($belongUserTypes == null?0:$belongUserTypes.hashCode());
      Object $name = this.getName();
      result = result * 59 + ($name == null?0:$name.hashCode());
      Object $desc = this.getDesc();
      result = result * 59 + ($desc == null?0:$desc.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 59 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 59 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "OrderActionInstance(id=" + this.getId() + ", nodeInstanceId=" + this.getNodeInstanceId() + ", action=" + this.getAction() + ", display=" + this.getDisplay() + ", type=" + this.getType() + ", belongUserTypes=" + this.getBelongUserTypes() + ", name=" + this.getName() + ", desc=" + this.getDesc() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }
}
