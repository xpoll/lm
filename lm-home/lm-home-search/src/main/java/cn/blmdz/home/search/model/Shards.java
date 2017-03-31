package cn.blmdz.home.search.model;

import java.io.Serializable;

public class Shards implements Serializable {
   private static final long serialVersionUID = 8970518308372594928L;
   private Integer total;
   private Integer successful;
   private Integer failed;

   public Integer getTotal() {
      return this.total;
   }

   public Integer getSuccessful() {
      return this.successful;
   }

   public Integer getFailed() {
      return this.failed;
   }

   public void setTotal(Integer total) {
      this.total = total;
   }

   public void setSuccessful(Integer successful) {
      this.successful = successful;
   }

   public void setFailed(Integer failed) {
      this.failed = failed;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Shards)) {
         return false;
      } else {
         Shards other = (Shards)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$total = this.getTotal();
            Object other$total = other.getTotal();
            if(this$total == null) {
               if(other$total != null) {
                  return false;
               }
            } else if(!this$total.equals(other$total)) {
               return false;
            }

            Object this$successful = this.getSuccessful();
            Object other$successful = other.getSuccessful();
            if(this$successful == null) {
               if(other$successful != null) {
                  return false;
               }
            } else if(!this$successful.equals(other$successful)) {
               return false;
            }

            Object this$failed = this.getFailed();
            Object other$failed = other.getFailed();
            if(this$failed == null) {
               if(other$failed != null) {
                  return false;
               }
            } else if(!this$failed.equals(other$failed)) {
               return false;
            }

            return true;
         }
      }
   }

   protected boolean canEqual(Object other) {
      return other instanceof Shards;
   }

   public int hashCode() {
      int PRIME = 59;
      int result = 1;
      Object $total = this.getTotal();
      result = result * 59 + ($total == null?0:$total.hashCode());
      Object $successful = this.getSuccessful();
      result = result * 59 + ($successful == null?0:$successful.hashCode());
      Object $failed = this.getFailed();
      result = result * 59 + ($failed == null?0:$failed.hashCode());
      return result;
   }

   public String toString() {
      return "Shards(total=" + this.getTotal() + ", successful=" + this.getSuccessful() + ", failed=" + this.getFailed() + ")";
   }
}
