package io.terminus.parana.msg.model;

import java.io.Serializable;
import java.util.Date;

public class MessageTemplate implements Serializable {
   private static final long serialVersionUID = -3001217040033432655L;
   private Long id;
   private Long creatorId;
   private String creatorName;
   private String name;
   private String title;
   private String content;
   private String context;
   private Integer channel;
   private Boolean disabled;
   private String description;
   private Date createdAt;
   private Date updatedAt;

   public Long getId() {
      return this.id;
   }

   public Long getCreatorId() {
      return this.creatorId;
   }

   public String getCreatorName() {
      return this.creatorName;
   }

   public String getName() {
      return this.name;
   }

   public String getTitle() {
      return this.title;
   }

   public String getContent() {
      return this.content;
   }

   public String getContext() {
      return this.context;
   }

   public Integer getChannel() {
      return this.channel;
   }

   public Boolean getDisabled() {
      return this.disabled;
   }

   public String getDescription() {
      return this.description;
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

   public void setCreatorId(Long creatorId) {
      this.creatorId = creatorId;
   }

   public void setCreatorName(String creatorName) {
      this.creatorName = creatorName;
   }

   public void setName(String name) {
      this.name = name;
   }

   public void setTitle(String title) {
      this.title = title;
   }

   public void setContent(String content) {
      this.content = content;
   }

   public void setContext(String context) {
      this.context = context;
   }

   public void setChannel(Integer channel) {
      this.channel = channel;
   }

   public void setDisabled(Boolean disabled) {
      this.disabled = disabled;
   }

   public void setDescription(String description) {
      this.description = description;
   }

   public void setCreatedAt(Date createdAt) {
      this.createdAt = createdAt;
   }

   public void setUpdatedAt(Date updatedAt) {
      this.updatedAt = updatedAt;
   }

   public String toString() {
      return "MessageTemplate(id=" + this.getId() + ", creatorId=" + this.getCreatorId() + ", creatorName=" + this.getCreatorName() + ", name=" + this.getName() + ", title=" + this.getTitle() + ", content=" + this.getContent() + ", context=" + this.getContext() + ", channel=" + this.getChannel() + ", disabled=" + this.getDisabled() + ", description=" + this.getDescription() + ", createdAt=" + this.getCreatedAt() + ", updatedAt=" + this.getUpdatedAt() + ")";
   }

   public boolean equals(Object o) {
      if(o == this) {
         return true;
      } else if(!(o instanceof MessageTemplate)) {
         return false;
      } else {
         MessageTemplate other = (MessageTemplate)o;
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

            return true;
         }
      }
   }

   public boolean canEqual(Object other) {
      return other instanceof MessageTemplate;
   }

   public int hashCode() {
      int PRIME = 31;
      int result = 1;
      Object $name = this.getName();
      result = result * 31 + ($name == null?0:$name.hashCode());
      return result;
   }
}
