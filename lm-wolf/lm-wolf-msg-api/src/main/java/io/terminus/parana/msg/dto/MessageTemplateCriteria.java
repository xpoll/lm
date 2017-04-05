package io.terminus.parana.msg.dto;

import io.terminus.parana.msg.dto.PagingCriteria;

public class MessageTemplateCriteria extends PagingCriteria {
   private static final long serialVersionUID = 872502999496820860L;
   private Integer channel;
   private Long creatorId;
   private String name;
   private String title;
   private Boolean disabled;

   public Integer getChannel() {
      return this.channel;
   }

   public Long getCreatorId() {
      return this.creatorId;
   }

   public String getName() {
      return this.name;
   }

   public String getTitle() {
      return this.title;
   }

   public Boolean getDisabled() {
      return this.disabled;
   }

   public void setChannel(Integer channel) {
      this.channel = channel;
   }

   public void setCreatorId(Long creatorId) {
      this.creatorId = creatorId;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setTitle(String title) {
      this.title = title;
   }

   public void setDisabled(Boolean disabled) {
      this.disabled = disabled;
   }

   public String toString() {
      return "MessageTemplateCriteria(channel=" + this.getChannel() + ", creatorId=" + this.getCreatorId() + ", name=" + this.getName() + ", title=" + this.getTitle() + ", disabled=" + this.getDisabled() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof MessageTemplateCriteria)) {
         return false;
      } else {
         MessageTemplateCriteria other = (MessageTemplateCriteria)o;
         if(!other.canEqual(this)) {
            return false;
         } else if(!super.equals(o)) {
            return false;
         } else {
            Object this$channel = this.getChannel();
            Object other$channel = other.getChannel();
            if(this$channel == null) {
               if(other$channel != null) {
                  return false;
               }
            } else if(!this$channel.equals(other$channel)) {
               return false;
            }

            Object this$creatorId = this.getCreatorId();
            Object other$creatorId = other.getCreatorId();
            if(this$creatorId == null) {
               if(other$creatorId != null) {
                  return false;
               }
            } else if(!this$creatorId.equals(other$creatorId)) {
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

            Object this$title = this.getTitle();
            Object other$title = other.getTitle();
            if(this$title == null) {
               if(other$title != null) {
                  return false;
               }
            } else if(!this$title.equals(other$title)) {
               return false;
            }

            Object this$disabled = this.getDisabled();
            Object other$disabled = other.getDisabled();
            if(this$disabled == null) {
               if(other$disabled != null) {
                  return false;
               }
            } else if(!this$disabled.equals(other$disabled)) {
               return false;
            }

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof MessageTemplateCriteria;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      result = result * 31 + super.hashCode();
      Object $channel = this.getChannel();
      result = result * 31 + ($channel == null?0:$channel.hashCode());
      Object $creatorId = this.getCreatorId();
      result = result * 31 + ($creatorId == null?0:$creatorId.hashCode());
      Object $name = this.getName();
      result = result * 31 + ($name == null?0:$name.hashCode());
      Object $title = this.getTitle();
      result = result * 31 + ($title == null?0:$title.hashCode());
      Object $disabled = this.getDisabled();
      result = result * 31 + ($disabled == null?0:$disabled.hashCode());
      return result;
   }
}
