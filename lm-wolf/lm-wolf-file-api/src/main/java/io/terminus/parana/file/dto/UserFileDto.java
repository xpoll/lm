package io.terminus.parana.file.dto;

import io.terminus.parana.file.model.UserFile;
import io.terminus.parana.file.model.UserFolder;
import java.io.Serializable;

public class UserFileDto implements Serializable {
   private UserFolder folder;
   private UserFile userFile;

   public UserFolder getFolder() {
      return this.folder;
   }

   public UserFile getUserFile() {
      return this.userFile;
   }

   public void setFolder(UserFolder folder) {
      this.folder = folder;
   }

   public void setUserFile(UserFile userFile) {
      this.userFile = userFile;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof UserFileDto)) {
         return false;
      } else {
         UserFileDto other = (UserFileDto)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$folder = this.getFolder();
            Object other$folder = other.getFolder();
            if(this$folder == null) {
               if(other$folder != null) {
                  return false;
               }
            } else if(!this$folder.equals(other$folder)) {
               return false;
            }

            Object this$userFile = this.getUserFile();
            Object other$userFile = other.getUserFile();
            if(this$userFile == null) {
               if(other$userFile != null) {
                  return false;
               }
            } else if(!this$userFile.equals(other$userFile)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof UserFileDto;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $folder = this.getFolder();
      result = result * 31 + ($folder == null?0:$folder.hashCode());
      Object $userFile = this.getUserFile();
      result = result * 31 + ($userFile == null?0:$userFile.hashCode());
      return result;
   }

   public String toString() {
      return "UserFileDto(folder=" + this.getFolder() + ", userFile=" + this.getUserFile() + ")";
   }
}
