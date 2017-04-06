package cn.blmdz.boot.datasource.autoconfigure.metadata;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.sql.DataSource;

import cn.blmdz.boot.datasource.autoconfigure.metadata.DataSourcePoolMetadata;
import cn.blmdz.boot.datasource.autoconfigure.metadata.DataSourcePoolMetadataProvider;

public class DataSourcePoolMetadataProviders implements DataSourcePoolMetadataProvider {
   private final List<DataSourcePoolMetadataProvider> providers;

   public DataSourcePoolMetadataProviders(Collection providers) {
      this.providers = (List)(providers == null?Collections.emptyList():new ArrayList(providers));
   }

   public DataSourcePoolMetadata getDataSourcePoolMetadata(DataSource dataSource) {
      for(DataSourcePoolMetadataProvider provider : this.providers) {
         DataSourcePoolMetadata metadata = provider.getDataSourcePoolMetadata(dataSource);
         if(metadata != null) {
            return metadata;
         }
      }

      return null;
   }
}
