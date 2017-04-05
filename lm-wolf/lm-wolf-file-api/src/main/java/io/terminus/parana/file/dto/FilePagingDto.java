package io.terminus.parana.file.dto;

import io.terminus.common.model.Paging;
import io.terminus.parana.file.dto.FileRealPath;
import java.io.Serializable;

public class FilePagingDto implements Serializable {
   private Paging fileDtoPaging;
   private FileRealPath fileRealPath;

   public Paging getFileDtoPaging() {
      return this.fileDtoPaging;
   }

   public FileRealPath getFileRealPath() {
      return this.fileRealPath;
   }

   public void setFileDtoPaging(Paging fileDtoPaging) {
      this.fileDtoPaging = fileDtoPaging;
   }

   public void setFileRealPath(FileRealPath fileRealPath) {
      this.fileRealPath = fileRealPath;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof FilePagingDto)) {
         return false;
      } else {
         FilePagingDto other = (FilePagingDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$fileDtoPaging = this.getFileDtoPaging();
            Object other$fileDtoPaging = other.getFileDtoPaging();
            if(this$fileDtoPaging == null) {
               if(other$fileDtoPaging != null) {
                  return false;
               }
            } else if(!this$fileDtoPaging.equals(other$fileDtoPaging)) {
               return false;
            }

            Object this$fileRealPath = this.getFileRealPath();
            Object other$fileRealPath = other.getFileRealPath();
            if(this$fileRealPath == null) {
               if(other$fileRealPath != null) {
                  return false;
               }
            } else if(!this$fileRealPath.equals(other$fileRealPath)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof FilePagingDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $fileDtoPaging = this.getFileDtoPaging();
      result = result * 31 + ($fileDtoPaging == null?0:$fileDtoPaging.hashCode());
      Object $fileRealPath = this.getFileRealPath();
      result = result * 31 + ($fileRealPath == null?0:$fileRealPath.hashCode());
      return result;
   }

   public String toString() {
      return "FilePagingDto(fileDtoPaging=" + this.getFileDtoPaging() + ", fileRealPath=" + this.getFileRealPath() + ")";
   }
}
