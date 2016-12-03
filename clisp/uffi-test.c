#include <stdlib.h>
#include <string.h>
#include <stdio.h>


    static char* stored_string=0;

    void uffi_test_string_in(const char* string)
    {
        if(stored_string!=0){
            free(stored_string);
        }
        stored_string=malloc(strlen(string)+1);
        strcpy(stored_string,string);
        printf("uffi_test_string_in: got string: %s\n",stored_string);
    }


    void uffi_test_string_out(char* string,unsigned int max_length)
    {
        unsigned int i;
        if(stored_string==0){
            for(i=0;i<max_length;i++){
                string[i]=(' '+(i%95));
            }
            string[i]=0;
        }else{
            for(i=0;(i<max_length)&&(stored_string[i]!=0);i++){
                string[i]=stored_string[i];
            }
            string[i]=0;
        }
        printf("uffi_test_string_out: filled string: %s\n",string);
        fflush(stdout);
    }


    const char* uffi_test_string_result(void)
    {
        const char* result="Hello World!";
        if(stored_string!=0){
            result=stored_string;
        }
        printf("uffi_test_string_result: returning string: %s\n",result);
        fflush(stdout);
        return(result);
    }






    static signed int stored_value=0;


    void uffi_test_sint32_in(const signed int value)
    {
        stored_value=value;
        printf("uffi_test_sint32_in: got integer: %d\n",stored_value);
        fflush(stdout);
    }


    void uffi_test_sint32_out(signed int* value)
    {
        (*value)=stored_value;
        printf("uffi_test_sint32_out: filled value: %d\n",*value);
        fflush(stdout);
    }


    signed int uffi_test_sint32_result(void)
    {
        printf("uffi_test_sint32_result: returning value: %d\n",stored_value);
        fflush(stdout);
        return(stored_value);
    }




    typedef struct {
        char    character;
        int     number;
        float   single_float;
        char    small_string[16];
        char*   big_string;
    }       record_t;


    void print_record(record_t* record)
    {
        char buffer[64];
        printf("record.character    = '%c'\n",record->character);
        printf("record.number       = %d\n",record->number);
        printf("record.single_float = %f\n",record->single_float);
        strncpy(buffer,record->small_string,sizeof(record->small_string));
        buffer[sizeof(record->small_string)]=0;
        printf("record.small_string = \"%s\"\n",buffer);
        if(record->big_string==0){
            printf("record.big_string   = NULL\n");
        }else{
            printf("record.big_string   = \"");
            fflush(stdout);
            write(fileno,record->big_string,strlen(record->big_string));
            printf("\"\n");
        }
    }


    static record_t stored_record;


    void uffi_test_record_in(const record_t* value)
    {
        stored_record.character=value->character;
        stored_record.number=value->number;
        stored_record.single_float=value->single_float;
        strncpy(stored_record.small_string,value->small_string,
                sizeof(stored_record.small_string));
        if(stored_record.big_string!=0){
            free(stored_record.big_string);
        }
        stored_record.big_string=malloc(strlen(value->big_string)+1);
        strcpy(stored_record.big_string,value->big_string);
        printf("uffi_test_record_in: got record:\n");
        print_record(&stored_record);
        fflush(stdout);
    }


    void uffi_test_record_out(record_t* value)
    {
        value->character=stored_record.character;
        value->number=stored_record.number;
        value->single_float=stored_record.single_float;
        strncpy(value->small_string,stored_record.small_string,
                sizeof(stored_record.small_string));
        value->big_string=malloc(strlen(stored_record.big_string)+1);
        strcpy(value->big_string,stored_record.big_string);
        printf("uffi_test_record_out: filled record: \n");
        print_record(value);
        fflush(stdout);
    }


    record_t* uffi_test_record_result(void)
    {
        printf("uffi_test_record_result: returning record:\n");
        print_record(&stored_record);
        fflush(stdout);
        return(&stored_record);
    }

/*** uffi-test.c                      --                     --          ***/
