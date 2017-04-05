package io.terminus.parana.file.dto;

import java.io.Serializable;
import java.util.List;

public class FileRealPath implements Serializable {
   private static final long serialVersionUID = -38331061924340967L;
   private String realPath;
   private List pathList;

   public String getRealPath() {
      return this.realPath;
   }

   public List getPathList() {
      return this.pathList;
   }

   public void setRealPath(String realPath) {
      this.realPath = realPath;
   }

   public void setPathList(List pathList) {
      this.pathList = pathList;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof FileRealPath)) {
         return false;
      } else {
         FileRealPath other = (FileRealPath)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$realPath = this.getRealPath();
            Object other$realPath = other.getRealPath();
            if(this$realPath == null) {
               if(other$realPath != null) {
                  return false;
               }
            } else if(!this$realPath.equals(other$realPath)) {
               return false;
            }

            Object this$pathList = this.getPathList();
            Object other$pathList = other.getPathList();
            if(this$pathList == null) {
               if(other$pathList != null) {
                  return false;
               }
            } else if(!this$pathList.equals(other$pathList)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof FileRealPath;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $realPath = this.getRealPath();
      result = result * 31 + ($realPath == null?0:$realPath.hashCode());
      Object $pathList = this.getPathList();
      result = result * 31 + ($pathList == null?0:$pathList.hashCode());
      return result;
   }

   public String toString() {
      return "FileRealPath(realPath=" + this.getRealPath() + ", pathList=" + this.getPathList() + ")";
   }
}
