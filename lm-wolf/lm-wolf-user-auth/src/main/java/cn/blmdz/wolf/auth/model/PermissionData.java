package cn.blmdz.wolf.auth.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.io.Serializable;
import java.util.List;

@JsonInclude(Include.NON_NULL)
public class PermissionData implements Serializable {
   private static final long serialVersionUID = 0L;
   private List requests;
   private List allRequests;
   private List resources;

   public List getRequests() {
      return this.requests;
   }

   public List getAllRequests() {
      return this.allRequests;
   }

   public List getResources() {
      return this.resources;
   }

   public void setRequests(List requests) {
      this.requests = requests;
   }

   public void setAllRequests(List allRequests) {
      this.allRequests = allRequests;
   }

   public void setResources(List resources) {
      this.resources = resources;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof PermissionData)) {
         return false;
      } else {
         PermissionData other = (PermissionData)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$requests = this.getRequests();
            Object other$requests = other.getRequests();
            if(this$requests == null) {
               if(other$requests != null) {
                  return false;
               }
            } else if(!this$requests.equals(other$requests)) {
               return false;
            }

            Object this$allRequests = this.getAllRequests();
            Object other$allRequests = other.getAllRequests();
            if(this$allRequests == null) {
               if(other$allRequests != null) {
                  return false;
               }
            } else if(!this$allRequests.equals(other$allRequests)) {
               return false;
            }

            Object this$resources = this.getResources();
            Object other$resources = other.getResources();
            if(this$resources == null) {
               if(other$resources != null) {
                  return false;
               }
            } else if(!this$resources.equals(other$resources)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof PermissionData;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $requests = this.getRequests();
      result = result * 31 + ($requests == null?0:$requests.hashCode());
      Object $allRequests = this.getAllRequests();
      result = result * 31 + ($allRequests == null?0:$allRequests.hashCode());
      Object $resources = this.getResources();
      result = result * 31 + ($resources == null?0:$resources.hashCode());
      return result;
   }

   public String toString() {
      return "PermissionData(requests=" + this.getRequests() + ", allRequests=" + this.getAllRequests() + ", resources=" + this.getResources() + ")";
   }
}
