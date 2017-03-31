package cn.blmdz.home.search.api.model;

import java.beans.ConstructorProperties;
import java.util.List;
import java.util.Map;

import cn.blmdz.home.search.model.Bucket;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@ToString
public class WithAggregations<T> extends Pagination<T> {
   private static final long serialVersionUID = 4349474162412383279L;
   @Getter
   @Setter
   private Long total;
   @Getter
   @Setter
   private List<T> data;
   @Getter
   @Setter
   private Map<String, List<Bucket>> aggregations;

   @ConstructorProperties({"total", "data", "aggregations"})
   public WithAggregations(Long total,
		List<T> data,
		Map<String, List<Bucket>> aggregations) {
	   super(total, data);
      this.total = total;
      this.data = data;
      this.aggregations = aggregations;
   }
}
