package io.terminus.parana.auth.model;

import java.io.Serializable;
import java.util.List;

public class DefaultRole implements Serializable {
   private static final long serialVersionUID = 0L;
   private String name;
   private String description;
   private List resources;
   private List requests;

   public String getName() {
      return this.name;
   }

   public String getDescription() {
      return this.description;
   }

   public List getResources() {
      return this.resources;
   }

   public List getRequests() {
      return this.requests;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setResources(List resources) {
      this.resources = resources;
   }

   public void setRequests(List requests) {
      this.requests = requests;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof DefaultRole)) {
         return false;
      } else {
         DefaultRole other = (DefaultRole)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$name = this.getName();
            Object other$name = other.getName();
            if(this$name == null) {
               if(other$name != null) {
                  return false;
               }
            } else if(!this$name.equals(other$name)) {
               return false;
            }

            Object this$description = this.getDescription();
            Object other$description = other.getDescription();
            if(this$description == null) {
               if(other$description != null) {
                  return false;
               }
            } else if(!this$description.equals(other$description)) {
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

            Object this$requests = this.getRequests();
            Object other$requests = other.getRequests();
            if(this$requests == null) {
               if(other$requests != null) {
                  return false;
               }
            } else if(!this$requests.equals(other$requests)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof DefaultRole;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $name = this.getName();
      result = result * 31 + ($name == null?0:$name.hashCode());
      Object $description = this.getDescription();
      result = result * 31 + ($description == null?0:$description.hashCode());
      Object $resources = this.getResources();
      result = result * 31 + ($resources == null?0:$resources.hashCode());
      Object $requests = this.getRequests();
      result = result * 31 + ($requests == null?0:$requests.hashCode());
      return result;
   }

   public String toString() {
      return "DefaultRole(name=" + this.getName() + ", description=" + this.getDescription() + ", resources=" + this.getResources() + ", requests=" + this.getRequests() + ")";
   }
}
