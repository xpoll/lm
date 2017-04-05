package io.terminus.parana.msg.dto;

import io.terminus.parana.msg.dto.PagingCriteria;
import java.util.Date;

public class MessageCriteria extends PagingCriteria {
   private static final long serialVersionUID = -5408223109605993529L;
   private Integer category;
   private String title;
   private Long senderId;
   private String sender;
   private String receiver;
   private Integer status;
   private Integer channel;
   private Boolean needSend;
   private Boolean needClosed;
   private Date startAtBefore;
   private Date startAtAfter;
   private Date endAtBefore;
   private Date endAtAfter;
   private Date sendAtBefore;
   private Date sendAtAfter;

   public Integer getCategory() {
      return this.category;
   }

   public String getTitle() {
      return this.title;
   }

   public Long getSenderId() {
      return this.senderId;
   }

   public String getSender() {
      return this.sender;
   }

   public String getReceiver() {
      return this.receiver;
   }

   public Integer getStatus() {
      return this.status;
   }

   public Integer getChannel() {
      return this.channel;
   }

   public Boolean getNeedSend() {
      return this.needSend;
   }

   public Boolean getNeedClosed() {
      return this.needClosed;
   }

   public Date getStartAtBefore() {
      return this.startAtBefore;
   }

   public Date getStartAtAfter() {
      return this.startAtAfter;
   }

   public Date getEndAtBefore() {
      return this.endAtBefore;
   }

   public Date getEndAtAfter() {
      return this.endAtAfter;
   }

   public Date getSendAtBefore() {
      return this.sendAtBefore;
   }

   public Date getSendAtAfter() {
      return this.sendAtAfter;
   }

   public void setCategory(Integer category) {
      this.category = category;
   }

   public void setTitle(String title) {
      this.title = title;
   }

   public void setSenderId(Long senderId) {
      this.senderId = senderId;
   }

   public void setSender(String sender) {
      this.sender = sender;
   }

   public void setReceiver(String receiver) {
      this.receiver = receiver;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public void setChannel(Integer channel) {
      this.channel = channel;
   }

   public void setNeedSend(Boolean needSend) {
      this.needSend = needSend;
   }

   public void setNeedClosed(Boolean needClosed) {
      this.needClosed = needClosed;
   }

   public void setStartAtBefore(Date startAtBefore) {
      this.startAtBefore = startAtBefore;
   }

   public void setStartAtAfter(Date startAtAfter) {
      this.startAtAfter = startAtAfter;
   }

   public void setEndAtBefore(Date endAtBefore) {
      this.endAtBefore = endAtBefore;
   }

   public void setEndAtAfter(Date endAtAfter) {
      this.endAtAfter = endAtAfter;
   }

   public void setSendAtBefore(Date sendAtBefore) {
      this.sendAtBefore = sendAtBefore;
   }

   public void setSendAtAfter(Date sendAtAfter) {
      this.sendAtAfter = sendAtAfter;
   }

   public String toString() {
      return "MessageCriteria(category=" + this.getCategory() + ", title=" + this.getTitle() + ", senderId=" + this.getSenderId() + ", sender=" + this.getSender() + ", receiver=" + this.getReceiver() + ", status=" + this.getStatus() + ", channel=" + this.getChannel() + ", needSend=" + this.getNeedSend() + ", needClosed=" + this.getNeedClosed() + ", startAtBefore=" + this.getStartAtBefore() + ", startAtAfter=" + this.getStartAtAfter() + ", endAtBefore=" + this.getEndAtBefore() + ", endAtAfter=" + this.getEndAtAfter() + ", sendAtBefore=" + this.getSendAtBefore() + ", sendAtAfter=" + this.getSendAtAfter() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof MessageCriteria)) {
         return false;
      } else {
         MessageCriteria other = (MessageCriteria)o;
         if(!other.canEqual(this)) {
            return false;
         } else if(!super.equals(o)) {
            return false;
         } else {
            Object this$category = this.getCategory();
            Object other$category = other.getCategory();
            if(this$category == null) {
               if(other$category != null) {
                  return false;
               }
            } else if(!this$category.equals(other$category)) {
               return false;
            }

            Object this$title = this.getTitle();
            Object other$title = other.getTitle();
            if(this$title == null) {
               if(other$title != null) {
                  return false;
               }
            } else if(!this$title.equals(other$title)) {
               return false;
            }

            Object this$senderId = this.getSenderId();
            Object other$senderId = other.getSenderId();
            if(this$senderId == null) {
               if(other$senderId != null) {
                  return false;
               }
            } else if(!this$senderId.equals(other$senderId)) {
               return false;
            }

            Object this$sender = this.getSender();
            Object other$sender = other.getSender();
            if(this$sender == null) {
               if(other$sender != null) {
                  return false;
               }
            } else if(!this$sender.equals(other$sender)) {
               return false;
            }

            Object this$receiver = this.getReceiver();
            Object other$receiver = other.getReceiver();
            if(this$receiver == null) {
               if(other$receiver != null) {
                  return false;
               }
            } else if(!this$receiver.equals(other$receiver)) {
               return false;
            }

            Object this$status = this.getStatus();
            Object other$status = other.getStatus();
            if(this$status == null) {
               if(other$status != null) {
                  return false;
               }
            } else if(!this$status.equals(other$status)) {
               return false;
            }

            Object this$channel = this.getChannel();
            Object other$channel = other.getChannel();
            if(this$channel == null) {
               if(other$channel != null) {
                  return false;
               }
            } else if(!this$channel.equals(other$channel)) {
               return false;
            }

            Object this$needSend = this.getNeedSend();
            Object other$needSend = other.getNeedSend();
            if(this$needSend == null) {
               if(other$needSend != null) {
                  return false;
               }
            } else if(!this$needSend.equals(other$needSend)) {
               return false;
            }

            Object this$needClosed = this.getNeedClosed();
            Object other$needClosed = other.getNeedClosed();
            if(this$needClosed == null) {
               if(other$needClosed != null) {
                  return false;
               }
            } else if(!this$needClosed.equals(other$needClosed)) {
               return false;
            }

            Object this$startAtBefore = this.getStartAtBefore();
            Object other$startAtBefore = other.getStartAtBefore();
            if(this$startAtBefore == null) {
               if(other$startAtBefore != null) {
                  return false;
               }
            } else if(!this$startAtBefore.equals(other$startAtBefore)) {
               return false;
            }

            Object this$startAtAfter = this.getStartAtAfter();
            Object other$startAtAfter = other.getStartAtAfter();
            if(this$startAtAfter == null) {
               if(other$startAtAfter != null) {
                  return false;
               }
            } else if(!this$startAtAfter.equals(other$startAtAfter)) {
               return false;
            }

            Object this$endAtBefore = this.getEndAtBefore();
            Object other$endAtBefore = other.getEndAtBefore();
            if(this$endAtBefore == null) {
               if(other$endAtBefore != null) {
                  return false;
               }
            } else if(!this$endAtBefore.equals(other$endAtBefore)) {
               return false;
            }

            Object this$endAtAfter = this.getEndAtAfter();
            Object other$endAtAfter = other.getEndAtAfter();
            if(this$endAtAfter == null) {
               if(other$endAtAfter != null) {
                  return false;
               }
            } else if(!this$endAtAfter.equals(other$endAtAfter)) {
               return false;
            }

            Object this$sendAtBefore = this.getSendAtBefore();
            Object other$sendAtBefore = other.getSendAtBefore();
            if(this$sendAtBefore == null) {
               if(other$sendAtBefore != null) {
                  return false;
               }
            } else if(!this$sendAtBefore.equals(other$sendAtBefore)) {
               return false;
            }

            Object this$sendAtAfter = this.getSendAtAfter();
            Object other$sendAtAfter = other.getSendAtAfter();
            if(this$sendAtAfter == null) {
               if(other$sendAtAfter != null) {
                  return false;
               }
            } else if(!this$sendAtAfter.equals(other$sendAtAfter)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof MessageCriteria;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      result = result * 31 + super.hashCode();
      Object $category = this.getCategory();
      result = result * 31 + ($category == null?0:$category.hashCode());
      Object $title = this.getTitle();
      result = result * 31 + ($title == null?0:$title.hashCode());
      Object $senderId = this.getSenderId();
      result = result * 31 + ($senderId == null?0:$senderId.hashCode());
      Object $sender = this.getSender();
      result = result * 31 + ($sender == null?0:$sender.hashCode());
      Object $receiver = this.getReceiver();
      result = result * 31 + ($receiver == null?0:$receiver.hashCode());
      Object $status = this.getStatus();
      result = result * 31 + ($status == null?0:$status.hashCode());
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $needSend = this.getNeedSend();
      result = result * 31 + ($needSend == null?0:$needSend.hashCode());
      Object $needClosed = this.getNeedClosed();
      result = result * 31 + ($needClosed == null?0:$needClosed.hashCode());
      Object $startAtBefore = this.getStartAtBefore();
      result = result * 31 + ($startAtBefore == null?0:$startAtBefore.hashCode());
      Object $startAtAfter = this.getStartAtAfter();
      result = result * 31 + ($startAtAfter == null?0:$startAtAfter.hashCode());
      Object $endAtBefore = this.getEndAtBefore();
      result = result * 31 + ($endAtBefore == null?0:$endAtBefore.hashCode());
      Object $endAtAfter = this.getEndAtAfter();
      result = result * 31 + ($endAtAfter == null?0:$endAtAfter.hashCode());
      Object $sendAtBefore = this.getSendAtBefore();
      result = result * 31 + ($sendAtBefore == null?0:$sendAtBefore.hashCode());
      Object $sendAtAfter = this.getSendAtAfter();
      result = result * 31 + ($sendAtAfter == null?0:$sendAtAfter.hashCode());
      return result;
   }
}
