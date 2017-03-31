package cn.blmdz.home.search.api.model;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.util.List;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@ToString
public class Pagination<T> implements Serializable {
   private static final long serialVersionUID = 7694181874896892599L;
   @Getter
   @Setter
   private Long total;
   @Getter
   @Setter
   private List<T> data;

   @ConstructorProperties({"total", "data"})
   public Pagination(Long total, List<T> data) {
      this.total = total;
      this.data = data;
   }
}
