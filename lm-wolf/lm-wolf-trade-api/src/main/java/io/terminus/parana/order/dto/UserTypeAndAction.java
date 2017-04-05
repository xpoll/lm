package io.terminus.parana.order.dto;

import java.io.Serializable;
import java.util.List;

public class UserTypeAndAction implements Serializable {
   private static final long serialVersionUID = -938467869395076691L;
   private Integer type;
   private List actionInstanceIds;

   public Integer getType() {
      return this.type;
   }

   public List getActionInstanceIds() {
      return this.actionInstanceIds;
   }

   public void setType(Integer type) {
      this.type = type;
   }

   public void setActionInstanceIds(List actionInstanceIds) {
      this.actionInstanceIds = actionInstanceIds;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof UserTypeAndAction)) {
         return false;
      } else {
         UserTypeAndAction other = (UserTypeAndAction)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$type = this.getType();
            Object other$type = other.getType();
            if(this$type == null) {
               if(other$type != null) {
                  return false;
               }
            } else if(!this$type.equals(other$type)) {
               return false;
            }

            Object this$actionInstanceIds = this.getActionInstanceIds();
            Object other$actionInstanceIds = other.getActionInstanceIds();
            if(this$actionInstanceIds == null) {
               if(other$actionInstanceIds != null) {
                  return false;
               }
            } else if(!this$actionInstanceIds.equals(other$actionInstanceIds)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof UserTypeAndAction;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $type = this.getType();
      result = result * 59 + ($type == null?0:$type.hashCode());
      Object $actionInstanceIds = this.getActionInstanceIds();
      result = result * 59 + ($actionInstanceIds == null?0:$actionInstanceIds.hashCode());
      return result;
   }

   public String toString() {
      return "UserTypeAndAction(type=" + this.getType() + ", actionInstanceIds=" + this.getActionInstanceIds() + ")";
   }
}
