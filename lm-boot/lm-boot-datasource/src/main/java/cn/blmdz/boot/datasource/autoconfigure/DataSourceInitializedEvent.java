package cn.blmdz.boot.datasource.autoconfigure;

import javax.sql.DataSource;
import org.springframework.context.ApplicationEvent;

public class DataSourceInitializedEvent extends ApplicationEvent {
   public DataSourceInitializedEvent(DataSource source) {
      super(source);
   }
}
