package cn.blmdz.wolf.msg.dto;

import java.io.Serializable;

public class MessageInfo implements Serializable {
   private static final long serialVersionUID = -3666690887946395658L;
   private String messageTitle;
   private String messageContent;

   public String getMessageTitle() {
      return this.messageTitle;
   }

   public String getMessageContent() {
      return this.messageContent;
   }

   public void setMessageTitle(String messageTitle) {
      this.messageTitle = messageTitle;
   }

   public void setMessageContent(String messageContent) {
      this.messageContent = messageContent;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof MessageInfo)) {
         return false;
      } else {
         MessageInfo other = (MessageInfo)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$messageTitle = this.getMessageTitle();
            Object other$messageTitle = other.getMessageTitle();
            if(this$messageTitle == null) {
               if(other$messageTitle != null) {
                  return false;
               }
            } else if(!this$messageTitle.equals(other$messageTitle)) {
               return false;
            }

            Object this$messageContent = this.getMessageContent();
            Object other$messageContent = other.getMessageContent();
            if(this$messageContent == null) {
               if(other$messageContent != null) {
                  return false;
               }
            } else if(!this$messageContent.equals(other$messageContent)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof MessageInfo;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $messageTitle = this.getMessageTitle();
      result = result * 31 + ($messageTitle == null?0:$messageTitle.hashCode());
      Object $messageContent = this.getMessageContent();
      result = result * 31 + ($messageContent == null?0:$messageContent.hashCode());
      return result;
   }

   public String toString() {
      return "MessageInfo(messageTitle=" + this.getMessageTitle() + ", messageContent=" + this.getMessageContent() + ")";
   }
}
