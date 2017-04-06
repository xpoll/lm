package cn.blmdz.boot.datasource.autoconfigure.metadata;

import javax.sql.DataSource;

import cn.blmdz.boot.datasource.autoconfigure.metadata.DataSourcePoolMetadata;

public interface DataSourcePoolMetadataProvider {
   DataSourcePoolMetadata getDataSourcePoolMetadata(DataSource var1);
}
