package io.terminus.parana.msg.model;

import com.google.common.base.Objects;
import java.io.Serializable;
import java.util.Date;

public class Message implements Serializable {
   private static final long serialVersionUID = 4864697120731446946L;
   private Long id;
   private String category;
   private String title;
   private String content;
   private String template;
   private String data;
   private String attaches;
   private String remark;
   private Long senderId;
   private String sender;
   private Date sendAt;
   private Date startAt;
   private Date endAt;
   private String receivers;
   private Integer groupMessageType;
   private Boolean checkSubscribe;
   private Integer status;
   private String failReason;
   private Integer channel;
   private String channelOutput;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public String getCategory() {
      return this.category;
   }

   public String getTitle() {
      return this.title;
   }

   public String getContent() {
      return this.content;
   }

   public String getTemplate() {
      return this.template;
   }

   public String getData() {
      return this.data;
   }

   public String getAttaches() {
      return this.attaches;
   }

   public String getRemark() {
      return this.remark;
   }

   public Long getSenderId() {
      return this.senderId;
   }

   public String getSender() {
      return this.sender;
   }

   public Date getSendAt() {
      return this.sendAt;
   }

   public Date getStartAt() {
      return this.startAt;
   }

   public Date getEndAt() {
      return this.endAt;
   }

   public String getReceivers() {
      return this.receivers;
   }

   public Integer getGroupMessageType() {
      return this.groupMessageType;
   }

   public Boolean getCheckSubscribe() {
      return this.checkSubscribe;
   }

   public Integer getStatus() {
      return this.status;
   }

   public String getFailReason() {
      return this.failReason;
   }

   public Integer getChannel() {
      return this.channel;
   }

   public String getChannelOutput() {
      return this.channelOutput;
   }

   public Date getCreatedAt() {
      return this.createdAt;
   }

   public Date getUpdatedAt() {
      return this.updatedAt;
   }

   public void setId(Long id) {
      this.id = id;
   }

   public void setCategory(String category) {
      this.category = category;
   }

   public void setTitle(String title) {
      this.title = title;
   }

   public void setContent(String content) {
      this.content = content;
   }

   public void setTemplate(String template) {
      this.template = template;
   }

   public void setData(String data) {
      this.data = data;
   }

   public void setAttaches(String attaches) {
      this.attaches = attaches;
   }

   public void setRemark(String remark) {
      this.remark = remark;
   }

   public void setSenderId(Long senderId) {
      this.senderId = senderId;
   }

   public void setSender(String sender) {
      this.sender = sender;
   }

   public void setSendAt(Date sendAt) {
      this.sendAt = sendAt;
   }

   public void setStartAt(Date startAt) {
      this.startAt = startAt;
   }

   public void setEndAt(Date endAt) {
      this.endAt = endAt;
   }

   public void setReceivers(String receivers) {
      this.receivers = receivers;
   }

   public void setGroupMessageType(Integer groupMessageType) {
      this.groupMessageType = groupMessageType;
   }

   public void setCheckSubscribe(Boolean checkSubscribe) {
      this.checkSubscribe = checkSubscribe;
   }

   public void setStatus(Integer status) {
      this.status = status;
   }

   public void setFailReason(String failReason) {
      this.failReason = failReason;
   }

   public void setChannel(Integer channel) {
      this.channel = channel;
   }

   public void setChannelOutput(String channelOutput) {
      this.channelOutput = channelOutput;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof Message)) {
         return false;
      } else {
         Message other = (Message)o;
         if(!other.canEqual(this)) {
            return false;
         } else {
            Object this$id = this.getId();
            Object other$id = other.getId();
            if(this$id == null) {
               if(other$id != null) {
                  return false;
               }
            } else if(!this$id.equals(other$id)) {
               return false;
            }

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

            Object this$content = this.getContent();
            Object other$content = other.getContent();
            if(this$content == null) {
               if(other$content != null) {
                  return false;
               }
            } else if(!this$content.equals(other$content)) {
               return false;
            }

            Object this$template = this.getTemplate();
            Object other$template = other.getTemplate();
            if(this$template == null) {
               if(other$template != null) {
                  return false;
               }
            } else if(!this$template.equals(other$template)) {
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

            Object this$attaches = this.getAttaches();
            Object other$attaches = other.getAttaches();
            if(this$attaches == null) {
               if(other$attaches != null) {
                  return false;
               }
            } else if(!this$attaches.equals(other$attaches)) {
               return false;
            }

            Object this$remark = this.getRemark();
            Object other$remark = other.getRemark();
            if(this$remark == null) {
               if(other$remark != null) {
                  return false;
               }
            } else if(!this$remark.equals(other$remark)) {
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

            Object this$sendAt = this.getSendAt();
            Object other$sendAt = other.getSendAt();
            if(this$sendAt == null) {
               if(other$sendAt != null) {
                  return false;
               }
            } else if(!this$sendAt.equals(other$sendAt)) {
               return false;
            }

            Object this$startAt = this.getStartAt();
            Object other$startAt = other.getStartAt();
            if(this$startAt == null) {
               if(other$startAt != null) {
                  return false;
               }
            } else if(!this$startAt.equals(other$startAt)) {
               return false;
            }

            Object this$endAt = this.getEndAt();
            Object other$endAt = other.getEndAt();
            if(this$endAt == null) {
               if(other$endAt != null) {
                  return false;
               }
            } else if(!this$endAt.equals(other$endAt)) {
               return false;
            }

            Object this$receivers = this.getReceivers();
            Object other$receivers = other.getReceivers();
            if(this$receivers == null) {
               if(other$receivers != null) {
                  return false;
               }
            } else if(!this$receivers.equals(other$receivers)) {
               return false;
            }

            Object this$groupMessageType = this.getGroupMessageType();
            Object other$groupMessageType = other.getGroupMessageType();
            if(this$groupMessageType == null) {
               if(other$groupMessageType != null) {
                  return false;
               }
            } else if(!this$groupMessageType.equals(other$groupMessageType)) {
               return false;
            }

            Object this$checkSubscribe = this.getCheckSubscribe();
            Object other$checkSubscribe = other.getCheckSubscribe();
            if(this$checkSubscribe == null) {
               if(other$checkSubscribe != null) {
                  return false;
               }
            } else if(!this$checkSubscribe.equals(other$checkSubscribe)) {
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

            Object this$failReason = this.getFailReason();
            Object other$failReason = other.getFailReason();
            if(this$failReason == null) {
               if(other$failReason != null) {
                  return false;
               }
            } else if(!this$failReason.equals(other$failReason)) {
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

            Object this$channelOutput = this.getChannelOutput();
            Object other$channelOutput = other.getChannelOutput();
            if(this$channelOutput == null) {
               if(other$channelOutput != null) {
                  return false;
               }
            } else if(!this$channelOutput.equals(other$channelOutput)) {
               return false;
            }

            Object this$createdAt = this.getCreatedAt();
            Object other$createdAt = other.getCreatedAt();
            if(this$createdAt == null) {
               if(other$createdAt != null) {
                  return false;
               }
            } else if(!this$createdAt.equals(other$createdAt)) {
               return false;
            }

            Object this$updatedAt = this.getUpdatedAt();
            Object other$updatedAt = other.getUpdatedAt();
            if(this$updatedAt == null) {
               if(other$updatedAt != null) {
                  return false;
               }
            } else if(!this$updatedAt.equals(other$updatedAt)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof Message;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $id = this.getId();
      result = result * 31 + ($id == null?0:$id.hashCode());
      Object $category = this.getCategory();
      result = result * 31 + ($category == null?0:$category.hashCode());
      Object $title = this.getTitle();
      result = result * 31 + ($title == null?0:$title.hashCode());
      Object $content = this.getContent();
      result = result * 31 + ($content == null?0:$content.hashCode());
      Object $template = this.getTemplate();
      result = result * 31 + ($template == null?0:$template.hashCode());
      Object $data = this.getData();
      result = result * 31 + ($data == null?0:$data.hashCode());
      Object $attaches = this.getAttaches();
      result = result * 31 + ($attaches == null?0:$attaches.hashCode());
      Object $remark = this.getRemark();
      result = result * 31 + ($remark == null?0:$remark.hashCode());
      Object $senderId = this.getSenderId();
      result = result * 31 + ($senderId == null?0:$senderId.hashCode());
      Object $sender = this.getSender();
      result = result * 31 + ($sender == null?0:$sender.hashCode());
      Object $sendAt = this.getSendAt();
      result = result * 31 + ($sendAt == null?0:$sendAt.hashCode());
      Object $startAt = this.getStartAt();
      result = result * 31 + ($startAt == null?0:$startAt.hashCode());
      Object $endAt = this.getEndAt();
      result = result * 31 + ($endAt == null?0:$endAt.hashCode());
      Object $receivers = this.getReceivers();
      result = result * 31 + ($receivers == null?0:$receivers.hashCode());
      Object $groupMessageType = this.getGroupMessageType();
      result = result * 31 + ($groupMessageType == null?0:$groupMessageType.hashCode());
      Object $checkSubscribe = this.getCheckSubscribe();
      result = result * 31 + ($checkSubscribe == null?0:$checkSubscribe.hashCode());
      Object $status = this.getStatus();
      result = result * 31 + ($status == null?0:$status.hashCode());
      Object $failReason = this.getFailReason();
      result = result * 31 + ($failReason == null?0:$failReason.hashCode());
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $channelOutput = this.getChannelOutput();
      result = result * 31 + ($channelOutput == null?0:$channelOutput.hashCode());
      Object $createdAt = this.getCreatedAt();
      result = result * 31 + ($createdAt == null?0:$createdAt.hashCode());
      Object $updatedAt = this.getUpdatedAt();
      result = result * 31 + ($updatedAt == null?0:$updatedAt.hashCode());
      return result;
   }

   public String toString() {
      return "Message(id=" + this.getId() + ", category=" + this.getCategory() + ", title=" + this.getTitle() + ", content=" + this.getContent() + ", template=" + this.getTemplate() + ", data=" + this.getData() + ", attaches=" + this.getAttaches() + ", remark=" + this.getRemark() + ", senderId=" + this.getSenderId() + ", sender=" + this.getSender() + ", sendAt=" + this.getSendAt() + ", startAt=" + this.getStartAt() + ", endAt=" + this.getEndAt() + ", receivers=" + this.getReceivers() + ", groupMessageType=" + this.getGroupMessageType() + ", checkSubscribe=" + this.getCheckSubscribe() + ", status=" + this.getStatus() + ", failReason=" + this.getFailReason() + ", channel=" + this.getChannel() + ", channelOutput=" + this.getChannelOutput() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public static enum GroupMessageType {
      NotGroupMessage(0, "普通消息"),
      ExcludedGroupMessage(1, "互斥群组消息"),
      NotExcluedGroupMessage(2, "非互斥群组消息");

      private final int value;
      private final String desc;

      private GroupMessageType(int number, String desc) {
         this.value = number;
         this.desc = desc;
      }

      public static Message.GroupMessageType from(int value) {
         for(Message.GroupMessageType type : values()) {
            if(Objects.equal(Integer.valueOf(type.value), Integer.valueOf(value))) {
               return type;
            }
         }

         return null;
      }

      public int value() {
         return this.value;
      }

      public String toString() {
         return this.desc;
      }
   }

   public static enum Status {
      Initial(0, "消息排队中"),
      SendSuccess(1, "发送成功"),
      SendFailed(-1, "发送失败"),
      Closed(2, "关闭"),
      InitialFailed(-2, "初始化消息失败");

      private final int value;
      private final String desc;

      private Status(int number, String desc) {
         this.value = number;
         this.desc = desc;
      }

      public static Message.Status from(int value) {
         for(Message.Status status : values()) {
            if(Objects.equal(Integer.valueOf(status.value), Integer.valueOf(value))) {
               return status;
            }
         }

         return null;
      }

      public int value() {
         return this.value;
      }

      public String toString() {
         return this.desc;
      }

      public static Boolean canChange(Integer from, Integer to) {
         return Boolean.valueOf(from.intValue() < to.intValue() || to.intValue() < 0);
      }
   }
}
