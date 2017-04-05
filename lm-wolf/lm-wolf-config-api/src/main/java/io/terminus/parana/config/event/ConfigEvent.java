package io.terminus.parana.config.event;

import com.google.common.base.Optional;
import java.io.Serializable;

public class ConfigEvent implements Serializable {
   private static final long serialVersionUID = 1L;
   private Optional op;
   private Optional data;

   public Optional getOp() {
      return this.op;
   }

   public Optional getData() {
      return this.data;
   }

   public void setOp(Optional op) {
      this.op = op;
   }

   public void setData(Optional data) {
      this.data = data;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof ConfigEvent)) {
         return false;
      } else {
         ConfigEvent other = (ConfigEvent)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$op = this.getOp();
            Object other$op = other.getOp();
            if(this$op == null) {
               if(other$op != null) {
                  return false;
               }
            } else if(!this$op.equals(other$op)) {
               return false;
            }

            Object this$data = this.getData();
            Object other$data = other.getData();
            if(this$data == null) {
               if(other$data != null) {
                  return false;
               }
            } else if(!this$data.equals(other$data)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof ConfigEvent;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $op = this.getOp();
      result = result * 31 + ($op == null?0:$op.hashCode());
      Object $data = this.getData();
      result = result * 31 + ($data == null?0:$data.hashCode());
      return result;
   }

   public String toString() {
      return "ConfigEvent(op=" + this.getOp() + ", data=" + this.getData() + ")";
   }
}
