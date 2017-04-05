package io.terminus.parana.auth.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.io.Serializable;
import java.util.List;

@JsonInclude(Include.NON_NULL)
public class CompiledTree implements Serializable {
   private static final long serialVersionUID = 0L;
   private String appKey;
   private String baseRole;
   private List children;

   public String getAppKey() {
      return this.appKey;
   }

   public String getBaseRole() {
      return this.baseRole;
   }

   public List getChildren() {
      return this.children;
   }

   public void setAppKey(String appKey) {
      this.appKey = appKey;
   }

   public void setBaseRole(String baseRole) {
      this.baseRole = baseRole;
   }

   public void setChildren(List children) {
      this.children = children;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof CompiledTree)) {
         return false;
      } else {
         CompiledTree other = (CompiledTree)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$appKey = this.getAppKey();
            Object other$appKey = other.getAppKey();
            if(this$appKey == null) {
               if(other$appKey != null) {
                  return false;
               }
            } else if(!this$appKey.equals(other$appKey)) {
               return false;
            }

            Object this$baseRole = this.getBaseRole();
            Object other$baseRole = other.getBaseRole();
            if(this$baseRole == null) {
               if(other$baseRole != null) {
                  return false;
               }
            } else if(!this$baseRole.equals(other$baseRole)) {
               return false;
            }

            Object this$children = this.getChildren();
            Object other$children = other.getChildren();
            if(this$children == null) {
               if(other$children != null) {
                  return false;
               }
            } else if(!this$children.equals(other$children)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof CompiledTree;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $appKey = this.getAppKey();
      result = result * 31 + ($appKey == null?0:$appKey.hashCode());
      Object $baseRole = this.getBaseRole();
      result = result * 31 + ($baseRole == null?0:$baseRole.hashCode());
      Object $children = this.getChildren();
      result = result * 31 + ($children == null?0:$children.hashCode());
      return result;
   }

   public String toString() {
      return "CompiledTree(appKey=" + this.getAppKey() + ", baseRole=" + this.getBaseRole() + ", children=" + this.getChildren() + ")";
   }

   @JsonInclude(Include.NON_NULL)
   public static class Node implements Serializable {
      private static final long serialVersionUID = 0L;
      private String key;
      private String name;
      private String description;
      private Boolean selected;
      private List children;

      public String getKey() {
         return this.key;
      }

      public String getName() {
         return this.name;
      }

      public String getDescription() {
         return this.description;
      }

      public Boolean getSelected() {
         return this.selected;
      }

      public List getChildren() {
         return this.children;
      }

      public void setKey(String key) {
         this.key = key;
      }

      public void setName(String name) {
         this.name = name;
      }

      public void setDescription(String description) {
         this.description = description;
      }

      public void setSelected(Boolean selected) {
         this.selected = selected;
      }

      public void setChildren(List children) {
         this.children = children;
      }

      public boolean equals(Object o) {
         if(o == this) {
            return true;
         } else if(!(o instanceof CompiledTree.Node)) {
            return false;
         } else {
            CompiledTree.Node other = (CompiledTree.Node)o;
            if(!other.canEqual(this)) {
               return false;
            } else {
               Object this$key = this.getKey();
               Object other$key = other.getKey();
               if(this$key == null) {
                  if(other$key != null) {
                     return false;
                  }
               } else if(!this$key.equals(other$key)) {
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

               Object this$description = this.getDescription();
               Object other$description = other.getDescription();
               if(this$description == null) {
                  if(other$description != null) {
                     return false;
                  }
               } else if(!this$description.equals(other$description)) {
                  return false;
               }

               Object this$selected = this.getSelected();
               Object other$selected = other.getSelected();
               if(this$selected == null) {
                  if(other$selected != null) {
                     return false;
                  }
               } else if(!this$selected.equals(other$selected)) {
                  return false;
               }

               Object this$children = this.getChildren();
               Object other$children = other.getChildren();
               if(this$children == null) {
                  if(other$children != null) {
                     return false;
                  }
               } else if(!this$children.equals(other$children)) {
                  return false;
               }

               return true;
            }
         }
      }

      public boolean canEqual(Object other) {
         return other instanceof CompiledTree.Node;
      }

      public int hashCode() {
         int PRIME = 31;
         int result = 1;
         Object $key = this.getKey();
         result = result * 31 + ($key == null?0:$key.hashCode());
         Object $name = this.getName();
         result = result * 31 + ($name == null?0:$name.hashCode());
         Object $description = this.getDescription();
         result = result * 31 + ($description == null?0:$description.hashCode());
         Object $selected = this.getSelected();
         result = result * 31 + ($selected == null?0:$selected.hashCode());
         Object $children = this.getChildren();
         result = result * 31 + ($children == null?0:$children.hashCode());
         return result;
      }

      public String toString() {
         return "CompiledTree.Node(key=" + this.getKey() + ", name=" + this.getName() + ", description=" + this.getDescription() + ", selected=" + this.getSelected() + ", children=" + this.getChildren() + ")";
      }
   }
}
